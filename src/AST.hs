module AST where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax

extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero
