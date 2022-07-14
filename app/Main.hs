{-# LANGUAGE FlexibleContexts #-}

module Main where

import CheckNonFinalMethodAttributes
import CheckNonPrivateAttributes
-- import qualified Data.ByteString
import Data.ByteString.Lazy.Internal
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
import System.FilePath.Find
import Text.Parsec.Error
import Control.Monad (MonadPlus (..))


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

findAllJavaFiles :: FilePath -> IO [FilePath]
findAllJavaFiles =
  find always (extension ==? ".java")


readAllFiles :: [FilePath] -> IO [(String, FilePath)]
readAllFiles =
  map (\path -> do (readFile path, path))

parseAllFiles :: [String] -> ([(Text.Parsec.Error.ParseError, path)],[(CompilationUnit, path)])
parseAllFiles files =
  let 
    parseAllfilesHelp fileList (errorList, cUnitList) =
      case fileList of
        [] ->
          parsedFileList
        (file, path) : restFiles ->
          case parse compilationUnit file of
            Left error ->
              parseAllfilesHelp restFiles ((error, path) : errorList, cUnitList)
            Right cUnit ->
              parseAllfilesHelp restFiles (errorList, (cUnit, path) : cUnitList)
    in parseAllfilesHelp files (mzero, mzero)




parseJava :: FilePath -> Bool -> IO ()
parseJava rootDir pretty =
  do
    pathList <- findAllJavaFiles rootDir
    fileList <- readAllFiles pathList
    let (errorResults, cUnitResults) = parseAllFiles fileList
    let diagnostics = map (\(cUnit, path) -> CheckNonFinalMethodAttributes.check cUnit path ++ CheckNonPrivateAttributes.check cUnit path ++ EmptyLoopBody.check cUnit path ++ NeedBraces.check cUnit path ++ NamingConventions.checkPackageName cUnit path) cUnitResults

    print
      ( Data.ByteString.Lazy.Internal.unpackChars
          ( RDF.encodetojson
              ( DiagnosticResult
                  { diagnostics = diagnostics,
                    resultSource = Just (Source {name = "jlint", sourceURL = Nothing}),
                    resultSeverity = RDF.checkSeverityList (map RDF.severity diagnostics) -- emmits highest severity of all results in all files
                  }
              )
          )
      )
