{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module NamingConventions where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)
import Text.RE.TDFA.String

{- Package Name -}

checkPackageName :: CompilationUnit -> FilePath -> [Diagnostic]
checkPackageName (CompilationUnit (Just pDeckl) _ _) = checkTLD (extractPackageNames pDeckl)
checkPackageName (CompilationUnit {}) = return []

checkTLD :: [String] -> FilePath -> [Diagnostic]
checkTLD [] _ = mzero
checkTLD (x : xs) path
  | matched $ x ?=~ [re|^[a-z]*$|] = checkRestPN xs path
  | otherwise = packageNameMsg x path ++ checkRestPN xs path

checkRestPN :: [String] -> FilePath -> [Diagnostic]
checkRestPN [] _ = mzero
checkRestPN (x : xs) path
  | matched $ x ?=~ [re|^[a-zA-Z_][a-zA-Z0-9_]*$|] = checkRestPN xs path
  | otherwise = packageNameMsg x path ++ checkRestPN xs path

packageNameMsg :: String -> FilePath -> [Diagnostic]
packageNameMsg name path = return (simpleDiagnostic ("PackageName element " ++ name ++ " does not match the specifications.") path)

extractPackageNames :: PackageDecl -> [String]
extractPackageNames pDeckl = do
  packageDecl <- universeBi pDeckl
  extractBody packageDecl
  where
    extractBody (Ident n) = return n
