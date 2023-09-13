module Main where

import CheckstyleXML (toRDF)
import Config (Rule)
import Control.Exception
import Control.Monad (MonadPlus (..), unless, when)
import Control.Monad.Extra (concatMapM)
import Data.Aeson (decodeFileStrict, eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Semigroup ((<>))
import Language.Java.Parser (compilationUnit, modifier, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Rules (checkWithConfig, defaultConfig)
import Language.Java.Syntax (CompilationUnit, Parsed)
import Options.Applicative
import RDF
import System.Directory
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
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
importJava (Params path showAST pretty maybeConfigFile Nothing) = do
  parseJava path pretty showAST maybeConfigFile []
importJava (Params path showAST pretty maybeConfigFile (Just checkstyleFile)) = do
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
  parseJava path pretty showAST maybeConfigFile diags

data Params = Params
  { path :: String,
    showAST :: Bool,
    pretty :: Bool,
    maybeConfigFile :: Maybe String,
    maybeCheckstyleFile :: Maybe String
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
          ( long "config"
              <> metavar "CONFIG-FILE"
              <> help "JSON file to configure the jlint rules."
          )
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

parseAllFiles :: [(String, FilePath)] -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit Parsed, FilePath)])
parseAllFiles files =
  let parseAllfilesHelp :: [(String, FilePath)] -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit Parsed, FilePath)]) -> ([(Text.Parsec.Error.ParseError, FilePath)], [(CompilationUnit Parsed, FilePath)])
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
parseJava :: FilePath -> Bool -> Bool -> Maybe FilePath -> [Diagnostic] -> IO ()
parseJava rootDir pretty showAST maybeConfigFile checkstyleDiags = do
  pathList <- findAllJavaFiles rootDir
  fileList <- readAllFiles pathList
  let (parsingErrors, cUnitResults) = parseAllFiles fileList
  if showAST
    then do
      print cUnitResults
    else do
      eitherConfig <- case maybeConfigFile of
        Just configFile -> do
          configExists <- doesFileExist configFile
          if configExists
            then do
              eitherConfig <- eitherDecodeFileStrict configFile :: IO (Either String [Rule])
              case eitherConfig of
                Left error -> return (Left ("Error beim parsen der Config-Datei: " ++ error ++ " Verwende Standard-Config."))
                Right config -> return (Right config)
            else do
              return (Left ("Angegebene Config-Datei " ++ configFile ++ " existiert nicht. Verwende Standard-Config."))
        Nothing -> return (Right defaultConfig)
      case eitherConfig of
        Left error -> do
          hPutStrLn stderr error
          exitFailure
        Right config -> do
          diagnostics <- concatMapM (uncurry (checkWithConfig config)) cUnitResults
          let parseErrors = map (\(parseError, path) -> RDF.simpleDiagnostic (show parseError) path) parsingErrors
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
