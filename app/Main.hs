{-# LANGUAGE FlexibleContexts #-}

module Main where

import CheckNonFinalMethodAttributes
import CheckNonPrivateAttributes
import Data.Semigroup ((<>))
import EmptyLoopBody (check)
import Language.Java.Parser (compilationUnit, modifier, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Syntax
import Lib
import NamingConventions
import NeedBraces
import Options.Applicative
import RDF
import UnusedLocalVariable

main :: IO ()
main = execParser opts >>= importJava
  where
    opts =
      info
        (params <**> helper)
        ( fullDesc
            <> progDesc "Parse the java file"
            <> header "Linter for java code"
        )

importJava :: Params -> IO ()
importJava (Params path pretty) = parseJava path pretty

data Params = Params
  { path :: String,
    pretty :: Bool
  }

params :: Parser Params
params =
  Params
    <$> strOption
      ( long "path"
          <> metavar "SRCPath"
          <> help "Path to the java file"
      )
    <*> switch
      ( long "pretty"
          <> help "By setting this Parameter the java source representation of the AST is shown"
      )

parseJava :: FilePath -> Bool -> IO ()
parseJava path pretty =
  let diagnosticsByRules cUnit = CheckNonFinalMethodAttributes.check cUnit path ++ CheckNonPrivateAttributes.check cUnit path ++ EmptyLoopBody.check cUnit path ++ NeedBraces.check cUnit path ++ UnusedLocalVariable.checkMethodVars cUnit path ++ UnusedLocalVariable.checkClassVars cUnit path
   in do
        input <- readFile path
        let result = parser compilationUnit input
        case result of
          Left error -> print error
          Right cUnit -> do
            if pretty then print (prettyPrint cUnit) else print cUnit
            print
              ( RDF.encodetojson
                  ( DiagnosticResult
                      { diagnostics = diagnosticsByRules cUnit,
                        resultSource = Just (Source {name = "jlint", sourceURL = Nothing}),
                        resultSeverity = RDF.checkSeverityList (map RDF.severity (diagnosticsByRules cUnit))
                      }
                  )
              )
