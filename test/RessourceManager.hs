module RessourceManager where

import Control.Monad (MonadPlus (..))
import Data.List.Split
import Data.Maybe
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import System.Directory
import Text.Parsec.Error
import UnliftIO.Exception

-- testsetup and lexer for dividing test sourcefils into fragments
{-
puts together name and sourcecode of each fragment, where the input has to be formated like:
  -------------------------
  FileName
  +++++++++++++++++++++++++
  Corresponding File (SourceCode)

-}

nameMarker :: String
nameMarker = "-------------------------"

codeMarker :: String
codeMarker = "+++++++++++++++++++++++++"

type FileName = String

type Code = String

setFileName :: Maybe String -> FileName
setFileName =
  fromMaybe "NameIsMissing"

setCode :: Maybe String -> Code
setCode =
  fromMaybe "CodeIsMissing"

fileLexer :: String -> FilePath -> [(String, FilePath)]
fileLexer input rootDir =
  firstStageLexer (splitInput input) mzero
  where
    splitInput :: String -> [String]
    splitInput input =
      concatMap (split (onSublist nameMarker)) (split (onSublist codeMarker) input)

    firstStageLexer :: [String] -> [(String, FilePath)] -> [(String, FilePath)]
    firstStageLexer input restults =
      case input of
        [] ->
          restults
        x : rest@(xs : xss) ->
          if x == nameMarker
            then secondStageLexer xss (mergeNameCode (setFileName (Just xs))) restults
            else
              if x == codeMarker
                then firstStageLexer xss (mergeNameCode xs (setCode Nothing) : restults)
                else firstStageLexer rest restults

    secondStageLexer :: [String] -> (String -> (String, FilePath)) -> [(String, FilePath)] -> [(String, FilePath)]
    secondStageLexer input emitResultFunc restults =
      case input of
        [] ->
          emitResultFunc (setCode Nothing) : restults
        x : rest@(xs : xss) ->
          if x == nameMarker
            then secondStageLexer xss (mergeNameCode (setFileName (Just xs))) (emitResultFunc (setCode Nothing) : restults)
            else
              if x == codeMarker
                then firstStageLexer xss (emitResultFunc (setCode (Just xs)) : restults)
                else secondStageLexer rest emitResultFunc restults

    mergeNameCode :: String -> String -> (String, FilePath)
    mergeNameCode fName sCode =
      (sCode, fName)

filterParsingResults :: [(Either ParseError CompilationUnit, FilePath)] -> [IO (CompilationUnit, FilePath)]
filterParsingResults =
  map
    ( \(paringResult, corresPath) -> case paringResult of
        Left error -> throwString (show error)
        Right cUnit -> return (cUnit, corresPath)
    )

parseJava :: String -> IO [IO (CompilationUnit, FilePath)]
parseJava path = do
  input <- readFile path
  let formatedInput = fileLexer input path
  let result =
        map
          ( \(inputCode, corresPath) -> do
              let eitherCUnit = parser compilationUnit inputCode
              (eitherCUnit, corresPath)
          )
          formatedInput

  return (filterParsingResults result)

withCUnit :: FilePath -> ([IO (CompilationUnit, FilePath)] -> IO [IO ()]) -> IO [IO ()]
withCUnit relativePath =
  bracket setupCUnit teardownCunit -- bracket before after during
  where
    setupCUnit :: IO [IO (CompilationUnit, FilePath)]
    setupCUnit =
      do
        path <- getCurrentDirectory
        parseJava (path ++ relativePath)

    teardownCunit :: [IO (CompilationUnit, FilePath)] -> IO [IO ()]
    teardownCunit _ = return [return ()]
