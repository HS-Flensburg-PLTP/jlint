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

data FileName a
  = SetName a
  | EmptyName a

data Code a 
  = SoureCode a
  | EmptyCode a


instance Monad FileName where
  (>>=) m f = case m of
                SetName a ->
                  f a
                EmptyName a->
                  f a 
  return = SetName 

instance Monad Code where
  (>>=) m f = case m of
                SoureCode a ->
                  f a
                EmptyCode a ->
                  f a
  return = SoureCode
                  



fileLexer :: String -> FilePath -> [(String, FilePath)]
fileLexer input rootDir =
  firstStageLexer (splitInput input) mzero
  where
    splitInput :: String -> [String]
    splitInput =
      split (onSublist "-------------------------" )
    

    firstStageLexer :: [String] -> [(String, FilePath)] -> [(String, FilePath)]
    firstStageLexer input restults =
      case input of
        [] ->
          restults
        x : rest@(xs : xss) ->
          if x == nameMarker then
            secondStageLexer xss (mergeNameCode (Right xs)) restults
          else if x == codeMarker then
            firstStageLexer xss (mergeNameCode xs (EmptyName "Missing Testfilename"): restults)
          else 
            firstStageLexer rest 

    secondStageLexer :: [String] -> (Monad m => m String -> [(String, FilePath)]) -> [(String, FilePath)] -> [(String, FilePath)]
    secondStageLexer input emitResultFunc restults =
      case input of
        [] ->
          emitResultFunc (EmptyCode "Code must have fallen off the page"): restults
        x : rest@(xs : xss) ->
          if x == nameMarker then
            secondStageLexer xss (mergeNameCode (SetName xs)) (emitResultFunc (EmptyCode "Code could not be found") : restults)
          else if x == codeMarker then
            firstStageLexer xss (emitResultFunc (SoureCode xs): restults)
          else 
            secondStageLexer rest emitResultFunc restults

    mergeNameCode :: Monad m => m String -> m String -> (String, FilePath)
    mergeNameCode maybeName maybeCode =
      maybeName >>=(\name -> maybeName >>= (\code -> (code, name)))


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
