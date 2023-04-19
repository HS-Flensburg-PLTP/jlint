module Main where

import CheckstyleXML (toRDF)
import Config
import Control.Monad (MonadPlus (..), unless, when)
import Data.Aeson (decodeFileStrict)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Semigroup ((<>))
import Language.Java.Parser (compilationUnit, modifier, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Rules (checkAll, checkRule)
import Language.Java.Syntax (CompilationUnit)
import Options.Applicative
import RDF
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.FilePath.Find (always, extension, find, (==?))
import System.IO (IOMode (ReadMode), char8, hGetContents, hPutStrLn, hSetEncoding, openFile, stderr)
import Text.Parsec.Error (ParseError)
import Text.XML.HaXml.Parse (xmlParse')

main :: IO ()
main = execParser opts >>= importJava
  where
    opts =
      info
        (params <**> helper)
        ( fullDesc
            <> progDesc "Runs linter checks on given <src-path> (.java FILE or all .java files in DIRECTORY)"
            <> header "Linter for java code"
        )

importJava :: Params -> IO ()
importJava (Params path showAST pretty Nothing) = do
  parseJava path pretty showAST []
importJava (Params path showAST pretty (Just checkstyleFile)) = do
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
  parseJava path pretty showAST diags

data Params = Params
  { path :: String,
    showAST :: Bool,
    pretty :: Bool,
    checkstyleFile :: Maybe String
  }

params :: Parser Params
params =
  Params
    <$> argument
      str
      (metavar "<src-path>")
    <*> switch
      ( long "show-ast"
          <> short 't'
          <> help "Only show AST of given file(s) - no checks run. Should only be used with single file."
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
            inputHandle <- openFile path ReadMode
            hSetEncoding inputHandle char8
            file <- hGetContents inputHandle
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
parseJava :: FilePath -> Bool -> Bool -> [Diagnostic] -> IO ()
parseJava rootDir pretty showAST checkstyleDiags =
  do
    pathList <- findAllJavaFiles rootDir
    fileList <- readAllFiles pathList
    let (parsingErrors, cUnitResults) = parseAllFiles fileList
    if showAST
      then do
        print cUnitResults
      else do
        config <- decodeFileStrict "config.json" :: IO (Maybe Config)
        case config of
          Nothing -> putStrLn "Fehler beim Laden der config File"
          Just config -> do
            let parseErrors = map (\(parseError, path) -> RDF.simpleDiagnostic (show parseError) path) parsingErrors
            let diagnostics = concatMap (uncurry (checkRule (rules config))) cUnitResults
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
            if numberOfHints == 0
              then do
                hPutStrLn stderr ("jlint did not generate any hints for the Java code in directory " ++ rootDir)
                exitSuccess
              else do
                hPutStrLn stderr ("jlint has generated " ++ show numberOfHints ++ " hint(s) for the Java code in directory " ++ rootDir)
                exitWith (ExitFailure numberOfHints)
