{-# LANGUAGE FlexibleContexts #-}
module Main where

import CheckNonFinalMethodAttributes (check)
import CheckNonPrivateAttributes (check)
import Data.Semigroup ((<>))
import Language.Java.Parser (compilationUnit, modifier, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Syntax
import Lib
import NeedBraces
import Options.Applicative
import RDF

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
  let diagnosticsByRules cUnit = CheckNonFinalMethodAttributes.check cUnit path ++ CheckNonPrivateAttributes.check cUnit path ++ NeedBraces.check cUnit path
   in do
        input <- readFile path
        let result = parser compilationUnit input
        case result of
          Left error -> print error
          Right cUnit -> do
            --if pretty then print (prettyPrint cUnit) else print cUnit
            print
              ( RDF.encodetojson
                  ( DiagnosticResult
                      { diagnostics = diagnosticsByRules cUnit,
                        resultSource = Just (Source {name = "jlint", sourceURL = Nothing}),
                        resultSeverity = (checkHighestSeverity (diagnosticsByRules cUnit) Nothing)
                      }
                  )
              )

checkHighestSeverity :: [Diagnostic] -> Maybe (Either String Int) -> Maybe (Either String Int)
checkHighestSeverity [] severity = severity
checkHighestSeverity (Diagnostic m _ s _ _ _ _ : xs) severity =
  if s == Just (Left "ERROR")
    then s
    else checkHighestSeverity xs s
