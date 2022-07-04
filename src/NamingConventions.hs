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
  | otherwise = return (simpleDiagnostic (packageNameMsg x) path) ++ checkRestPN xs path

checkRestPN :: [String] -> FilePath -> [Diagnostic]
checkRestPN list path = map (\x -> simpleDiagnostic (packageNameMsg x) path) (filter (\x -> not (matched $ x ?=~ [re|^[a-zA-Z_][a-zA-Z0-9_]*$|])) list)

packageNameMsg :: String -> String
packageNameMsg name = "PackageName element " ++ name ++ " does not match the specifications."

extractPackageNames :: PackageDecl -> [String]
extractPackageNames pDeckl = do
  packageDecl <- universeBi pDeckl
  extractBody packageDecl
  where
    extractBody (Ident n) = return n
