{-# LANGUAGE FlexibleContexts #-}

module Main where

import CheckstyleXML (toRDF)
import Control.Monad (MonadPlus (..), unless, when)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Semigroup ((<>))
import Language.Java.Parser (compilationUnit, modifier, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Rules (checkAll)
import Language.Java.Syntax (CompilationUnit)
import Options.Applicative
import RDF
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath.Find (always, extension, find, (==?))
import System.IO (hPutStrLn, stderr)
import Text.Parsec.Error (ParseError)
import Text.XML.HaXml.Parse (xmlParse')

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
importJava (Params path pretty Nothing) = do
  parseJava path pretty []
importJava (Params path pretty (Just checkstyleFile)) = do
  content <- readFile checkstyleFile
  diags <- case xmlParse' "" content of
    Left error -> do
      hPutStrLn stderr ("Parsing checkstyle XML failed with error " ++ error)
      return []
    Right xml -> case CheckstyleXML.toRDF xml of
      Left error -> do
        hPutStrLn stderr ("Import of checkstyle XML failed with error " ++ error)
        return []
      Right diags -> return diags
  parseJava path pretty diags

data Params = Params
  { path :: String,
    pretty :: Bool,
    checkstyleFile :: Maybe String
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
    <*> optional
      ( strOption
          ( long "checkstyle"
              <> metavar "CheckstyleXMLFile"
              <> help "File with result of checkstyle application"
          )
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
            readAllFilesHelp restPaths ((file, path) : fileList)
   in readAllFilesHelp paths mzero

parseAllFiles :: [(String, FilePath)] -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit, FilePath)])
parseAllFiles files =
  let parseAllfilesHelp :: [(String, FilePath)] -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit, FilePath)]) -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit, FilePath)])
      parseAllfilesHelp fileList (errorList, cUnitList) =
        case fileList of
          [] ->
            (errorList, cUnitList)
          (file, path) : restFiles ->
            case parser compilationUnit path file of
              Left error ->
                parseAllfilesHelp restFiles ((error, path) : errorList, cUnitList)
              Right cUnit ->
                parseAllfilesHelp restFiles (errorList, (cUnit, path) : cUnitList)
   in parseAllfilesHelp files (mzero, mzero)

-- missing pretty option and does not print errors
parseJava :: FilePath -> Bool -> [Diagnostic] -> IO ()
parseJava rootDir pretty checkstyleDiags =
  do
    pathList <- findAllJavaFiles rootDir
    fileList <- readAllFiles pathList
    let (parsingErrors, cUnitResults) = parseAllFiles fileList
    let parseErrors = map (\(parseError, path) -> RDF.simpleDiagnostic (show parseError) path) parsingErrors
    let diagnostics = concatMap (uncurry checkAll) cUnitResults
    let diagnosticResults = checkstyleDiags ++ diagnostics ++ parseErrors
    C.putStrLn
      ( RDF.encodetojson
          ( DiagnosticResult
              { diagnostics = diagnosticResults,
                resultSource = Just (Source {name = "jlint", url = Nothing}),
                resultSeverity = RDF.checkSeverityList (map RDF.severity diagnosticResults) -- emmits highest severity of all results in all files
              }
          )
      )
    unless (null parsingErrors) $ print parsingErrors
    when pretty $ putStrLn (unlines (map (\(cUnit, _) -> prettyPrint cUnit) cUnitResults))

    let numberOfHints = length diagnostics
    putStrLn ("jlint has generated " ++ show numberOfHints ++ " hint(s) for the provided Java code")
    exitWith (ExitFailure numberOfHints)