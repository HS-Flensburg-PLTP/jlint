module RessourceManager where

import Control.Monad (MonadPlus (..))
import Data.Char (toLower)
import Data.List.Split
import Data.Maybe
import Data.String.Utils (strip)
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import System.Directory
import System.FilePath.Posix (addExtension, combine, dropExtension, takeExtension, (</>))
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

fileLexer :: String -> FilePath -> [(String, FilePath)]
fileLexer input rootDir =
  firstStageLexer (splitInput input) mzero
  where
    splitInput :: String -> [String]
    splitInput input =
      concatMap (split (dropInitBlank $ onSublist nameMarker)) (split (dropInitBlank $ onSublist codeMarker) input)

    setFileName :: Maybe FileName -> FileName
    setFileName maybeName =
      case maybeName of
        Just name ->
          addExtension (dropExtension rootDir </> strip name) (takeExtension rootDir)
        Nothing ->
          addExtension (dropExtension rootDir </> "NameNotDefined") (takeExtension rootDir)

    setCode :: Maybe String -> Code
    setCode =
      fromMaybe "CodeIsMissing"

    -- code without flags is interpreted as code input. This is for reasons of convience, in case there is only one inputfile
    firstStageLexer :: [String] -> [(String, FilePath)] -> [(String, FilePath)]
    firstStageLexer input results =
      case input of
        [] ->
          results
        x : rest@(xs : xss) ->
          if x == nameMarker then secondStageLexer xss (mergeNameCode (setFileName (Just xs))) results
          else if x == codeMarker then firstStageLexer xss (mergeNameCode (setFileName Nothing) xs : results)
          else firstStageLexer rest results
        x : _ ->
          if x /= nameMarker && x /= codeMarker then mergeNameCode (setFileName (Just "Testfile")) (setCode (Just x)) : results
          else results


    secondStageLexer :: [String] -> (String -> (String, FilePath)) -> [(String, FilePath)] -> [(String, FilePath)]
    secondStageLexer input emitResultFunc results =
      case input of
        [] ->
          emitResultFunc (setCode Nothing) : results
        x : rest@(xs : xss) ->
          if x == nameMarker then secondStageLexer xss (mergeNameCode (setFileName (Just xs))) (emitResultFunc (setCode Nothing) : results)
          else if x == codeMarker then firstStageLexer xss (emitResultFunc (setCode (Just xs)) : results)
          else secondStageLexer rest emitResultFunc results
        x : _ ->
          if x /= nameMarker && x /= codeMarker then emitResultFunc (setCode (Just x)) : results
          else results

    mergeNameCode :: String -> String -> (String, FilePath)
    mergeNameCode fName sCode =
      (sCode, fName)

filterParsingResults :: [(Either ParseError CompilationUnit, FilePath)] -> [IO (CompilationUnit, FilePath)]
filterParsingResults =
  map
    ( \(paringResult, corresPath) -> case paringResult of
        Left error -> throwString (show error ++ "\n" ++ corresPath)
        Right cUnit -> return (cUnit, corresPath)
    )

parseJava :: FilePath -> IO [IO (CompilationUnit, FilePath)]
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
