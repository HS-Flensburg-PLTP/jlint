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

subDivideInput :: String -> FilePath -> [(String, FilePath)]
subDivideInput input rootDir =
  composeOutput False (splitInput input) mzero
  where
    splitInput :: String -> [String]
    splitInput =
      split (onSublist "-------------------------")

    composeOutput :: Bool -> [String] -> [(String, FilePath)] -> [(String, FilePath)]
    composeOutput startFlag fragmentList fileList =
      case fragmentList of
        [] ->
          fileList
        fragmentName : restFragA@(_ : (fragmentCode : restFragB)) ->
          if startFlag
            then composeOutput False restFragB ((fragmentCode, fragmentName) : fileList)
            else composeOutput True restFragA fileList
        fragmentName : _ ->
         ("" , fragmentName) : fileList

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
  let formatedInput = subDivideInput input path
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
