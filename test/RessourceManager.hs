module RessourceManager where

import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import System.Directory
import UnliftIO.Exception

-- parse Java if fail
parseJava :: String -> IO CompilationUnit
parseJava path = do
  input <- readFile path
  let result = parser compilationUnit input
  case result of
    Left error -> throwString (show error) -- throwString :: (MonadIO m, HasCallStack) => String -> m a
    Right cUnit -> return cUnit

withCUnit :: String -> ((FilePath, CompilationUnit) -> IO ()) -> IO ()
withCUnit fPath =
  bracket (setupCUnit fPath) teardownCunit -- bracket before after during
  where
    setupCUnit :: FilePath -> IO (FilePath, CompilationUnit)
    setupCUnit pathFromRoot = do
      path <- getCurrentDirectory
      cUnit <- parseJava (path ++ pathFromRoot)
      return (path ++ pathFromRoot, cUnit)

    teardownCunit :: (FilePath, CompilationUnit) -> IO ()
    teardownCunit _ = return ()
