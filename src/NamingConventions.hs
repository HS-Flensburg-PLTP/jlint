{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module NamingConventions where

import Data.Function ((&))
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)
import Text.RE.TDFA.String

{- Package Name -}

checkPackageName :: CompilationUnit -> FilePath -> [Diagnostic]
checkPackageName (CompilationUnit (Just pDeckl) _ _) path = checkTLD (extractPackageNames pDeckl) path
checkPackageName (CompilationUnit {}) _ = []

checkTLD :: [String] -> FilePath -> [Diagnostic]
checkTLD [] _ = []
checkTLD (ident : idents) path
  | checkREOne ident = checkRestPN idents path
  | otherwise = simpleDiagnostic (packageNameMsg ident) path : checkRestPN idents path

checkRestPN :: [String] -> FilePath -> [Diagnostic]
checkRestPN idents path =
  idents
    & filter (not . checkRETwo)
    & map (\ident -> simpleDiagnostic (packageNameMsg ident) path)

packageNameMsg :: String -> String
packageNameMsg name = "PackageName element " ++ name ++ " does not match the specifications."

extractPackageNames :: PackageDecl -> [String]
extractPackageNames (PackageDecl (Name packageNames)) = map (\(Ident name) -> name) packageNames

checkREOne :: String -> Bool
checkREOne ident = matched (ident ?=~ [re|^[a-z]*$|])

checkRETwo :: String -> Bool
checkRETwo ident = matched (ident ?=~ [re|^[a-zA-Z_][a-zA-Z0-9_]*$|])
