module RessourceManager where

import Data.List.Split
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import System.Directory
import Text.Parsec.Error
import UnliftIO.Exception
import Control.Monad (MonadPlus (..))


-- divide input

{-
puts together name and sourcecode of each fragment, where the input has to be formated like:
  -------------------------
  FileName
  -------------------------
  Corresponding File (SourceCode)

-}
nameMarker :: String
nameMarker = "-------------------------" 

codeMarker :: String
codeMarker = "+++++++++++++++++++++++++"

type FileName = String

setFileName :: Maybe String -> FileName
setFileName maybeName =
 case maybeName of
    Just name ->
      name
    Nothing ->
      "NameIsMissing"

type Code = String

setCode :: Maybe String -> Code
setCode maybeName =
 case maybeName of
    Just code ->
      code
    Nothing ->
      "CodeIsMissing"

                  

fileLexer :: String -> FilePath -> [(String, FilePath)]
fileLexer input rootDir =
  firstStageLexer (splitInput input) mzero
  where
    splitInput :: String -> [String]
    splitInput input =
      concat (map (\inputSeg -> split (onSublist nameMarker) inputSeg) (split (onSublist codeMarker) input))
    

    firstStageLexer :: [String] -> [(String, FilePath)] -> [(String, FilePath)]
    firstStageLexer input restults =
      case input of
        [] ->
          restults
        x : rest@(xs : xss) ->
          if x == nameMarker then
            secondStageLexer xss (mergeNameCode (setFileName (Just xs))) restults
          else if x == codeMarker then
            firstStageLexer xss (mergeNameCode xs (setCode Nothing): restults)
          else 
            firstStageLexer rest restults

    secondStageLexer :: [String] -> (String -> (String, FilePath)) -> [(String, FilePath)] -> [(String, FilePath)]
    secondStageLexer input emitResultFunc restults =
      case input of
        [] ->
          emitResultFunc (setCode Nothing): restults
        x : rest@(xs : xss) ->
          if x == nameMarker then
            secondStageLexer xss (mergeNameCode (setFileName (Just xs))) (emitResultFunc (setCode Nothing) : restults)
          else if x == codeMarker then
            firstStageLexer xss (emitResultFunc (setCode (Just xs)) : restults)
          else 
            secondStageLexer rest emitResultFunc restults

    mergeNameCode :: String -> String -> (String, FilePath)
    mergeNameCode fName sCode =
      (sCode, fName)


filterParsingResults :: [(Either ParseError CompilationUnit, FilePath)] -> [IO(CompilationUnit, FilePath)]
filterParsingResults =
  map
    ( \(paringResult, corresPath) -> case paringResult of
        Left error -> throwString (show error)
        Right cUnit -> return (cUnit, corresPath)
    )

-- parse Java if fail
parseJava :: String -> IO [IO(CompilationUnit, FilePath)] -- [IO CompilationUnit]
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
