{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module PackageName where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)
import Text.RE.TDFA.String

check :: CompilationUnit -> FilePath -> [Diagnostic]
check (CompilationUnit (Just pDeckl) _ _) = checkTLD (extractNames pDeckl)
check (CompilationUnit {}) = return []

checkTLD :: [String] -> FilePath -> [Diagnostic]
checkTLD [] _ = mzero
checkTLD (x : xs) path
  | matched $ x ?=~ [re|^[a-z]*$|] = checkRest xs path
  | otherwise = checkNames x path ++ checkRest xs path

checkRest :: [String] -> FilePath -> [Diagnostic]
checkRest [] _ = mzero
checkRest (x : xs) path
  | matched $ x ?=~ [re|^[a-zA-Z_][a-zA-Z0-9_]*$|] = checkRest xs path
  | otherwise = checkNames x path ++ checkRest xs path

checkNames :: String -> FilePath -> [Diagnostic]
checkNames name path = return (simpleDiagnostic ("PackageName element " ++ name ++ " does not match the specifications.") path)

extractNames :: PackageDecl -> [String]
extractNames pDeckl = do
  packageDecl <- universeBi pDeckl
  extractBody packageDecl
  where
    extractBody (Ident n) = return n
