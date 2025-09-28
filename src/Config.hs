module Config (default_, checkWith) where

import Control.Monad.Extra (concatMapM)
import Language.Java.Syntax (CompilationUnit, Parsed)
import qualified RDF
import Rule (Rule (..))

type Config = [Rule]

default_ :: Config
default_ = []

checkWith :: Config -> CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
checkWith config cUnit filePath =
  concatMapM (\rule -> Rule.check rule cUnit filePath) config
