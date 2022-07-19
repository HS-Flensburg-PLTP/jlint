{-# LANGUAGE FlexibleContexts #-}

module Main where

import CheckNonFinalMethodAttributes
import CheckNonPrivateAttributes
-- import qualified Data.ByteString
import Control.Monad (MonadPlus (..), unless)
import Data.ByteString.Lazy.Internal
import Data.Semigroup ((<>))
import EmptyLoopBody
import Language.Java.Parser (compilationUnit, modifier, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Syntax
import Lib
import NamingConventions
import NeedBraces
import Options.Applicative
import RDF
import Rules
import System.FilePath.Find
import Text.Parsec.Error

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

-- can be done with map, does not continue when one file fails to be found
readAllFiles :: [FilePath] -> IO [(String, FilePath)]
readAllFiles paths =
  let readAllFilesHelp :: [FilePath] -> [(String, FilePath)] -> IO [(String, FilePath)]
      readAllFilesHelp pathList fileList =
        case pathList of
          [] ->
            return fileList
          path : restPaths -> do
            file <- readFile path
            let fileres = (file, path)
            readAllFilesHelp restPaths (return fileres)
   in readAllFilesHelp paths mzero

parseAllFiles :: [(String, FilePath)] -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit, FilePath)])
parseAllFiles files =
  let parseAllfilesHelp :: [(String, FilePath)] -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit, FilePath)]) -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit, FilePath)])
      parseAllfilesHelp fileList (errorList, cUnitList) =
        case fileList of
          [] ->
            (errorList, cUnitList)
          (file, path) : restFiles ->
            case parser compilationUnit file of
              Left error ->
                parseAllfilesHelp restFiles ((error, path) : errorList, cUnitList)
              Right cUnit ->
                parseAllfilesHelp restFiles (errorList, (cUnit, path) : cUnitList)
   in parseAllfilesHelp files (mzero, mzero)

--missing pretty option and does not print errors
parseJava :: FilePath -> Bool -> IO ()
parseJava rootDir pretty =
  do
    pathList <- findAllJavaFiles rootDir
    fileList <- readAllFiles pathList
    let (parsingErrors, cUnitResults) = parseAllFiles fileList
    if pretty
      then putStrLn (unlines (map (\(cUnit, _) -> prettyPrint cUnit) cUnitResults))
      else do
        let diagnostics = concatMap (\(cUnit, path) -> CheckNonFinalMethodAttributes.check cUnit path ++ CheckNonPrivateAttributes.check cUnit path ++ EmptyLoopBody.check cUnit path ++ NeedBraces.check cUnit path ++ NamingConventions.checkPackageName cUnit path) cUnitResults
        putStrLn
          ( Data.ByteString.Lazy.Internal.unpackChars
              ( RDF.encodetojson
                  ( DiagnosticResult
                      { diagnostics = diagnostics,
                        resultSource = Just (Source {name = "jlint", url = Nothing}),
                        resultSeverity = RDF.checkSeverityList (map RDF.severity diagnostics) -- emmits highest severity of all results in all files
                      }
                  )
              )
          )
        unless (null parsingErrors) $ print parsingErrors
