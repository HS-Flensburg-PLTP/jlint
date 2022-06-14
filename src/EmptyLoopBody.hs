{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EmptyLoopBody where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path =
  let fBlocks = extractMethods cUnit

      search [] = []
      search ((n, b) : xs) = extractStatements b n path ++ search xs
   in search fBlocks

extractMethods :: CompilationUnit -> [(String, MethodBody)]
extractMethods cUnit = do
  membDecl <- universeBi cUnit
  extractBody membDecl
  where
    extractBody (MethodDecl _ _ _ (Ident n) _ _ _ b) = return (n, b)
    extractBody _ = mzero

extractStatements :: MethodBody -> String -> FilePath -> [Diagnostic]
extractStatements methodBody funcName path = do
  stmt <- universeBi methodBody
  extractStatement stmt
  where
    extractStatement (Do (Empty) _) = return (simpleDiagnostic (msg "A Do-Loop" funcName) path)
    extractStatement (While _ (Empty)) = return (simpleDiagnostic (msg "A While-Loop" funcName) path)
    extractStatement (BasicFor _ _ _ (Empty)) = return (simpleDiagnostic (msg "A For-Loop" funcName) path)
    extractStatement _ = mzero

msg :: String -> String -> String
msg t funcName = t ++ " in function " ++ funcName ++ " contains no braces."
