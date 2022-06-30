{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module PackageName where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit (Just pDeckl) _ _) = checkTLD (extractNames pDeckl)
check (CompilationUnit {}) = return []

checkTLD :: [String] -> FilePath -> [Diagnostic]
checkTLD [] path = mzero
checkTLD (x : xs) path =
  if matched $ x ?=~ [re|^[a-z]*$|]
    then checkRest xs path
    else checkNames x path ++ checkRest xs path

checkRest :: [String] -> FilePath -> [Diagnostic]
checkRest [] path = mzero
checkRest (x : xs) path =
  if matched $ x ?=~ [re|^[a-zA-Z_][a-zA-Z0-9_]*$|]
    then checkRest xs path
    else checkNames x path ++ checkRest xs path

checkNames :: String -> FilePath -> [Diagnostic]
checkNames name path = return (simpleDiagnostic ("PackageName element " ++ name ++ " does not match the specifications.") path)

extractNames :: PackageDecl -> [String]
extractNames pDeckl = do
  packageDecl <- universeBi pDeckl
  extractBody packageDecl
  where
    extractBody (Ident n) = return n
    extractBody _ = mzero
