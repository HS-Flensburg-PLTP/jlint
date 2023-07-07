{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.PredictMethodNames where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF
import System.Process

whitelist :: [String]
whitelist = [""]

data DictLanguage
  = DE
  | EN

resolveDict :: DictLanguage -> String
resolveDict DE = "de_DE"
resolveDict EN = "en_US"

predictMethodNames :: MemberDecl Parsed -> IO String
predictMethodNames methodDecl = do
  writeToFile methodDecl
  readProcess "python3" ["--predict"] ""

{- or to json -}
writeToFile :: MemberDecl Parsed -> IO ()
writeToFile methodDecl@(MethodDecl {}) = mzero
writeToFile _ = mzero

check :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  let memberDecls = universeBi cUnit
  checkMemberDecls memberDecls path

checkMemberDecls :: [MemberDecl Parsed] -> FilePath -> IO [RDF.Diagnostic]
checkMemberDecls [] _ = return []
checkMemberDecls (memberDecl : memberDecls) path = do
  result <- checkMemberDecl memberDecl path
  results <- checkMemberDecls memberDecls path
  return (result ++ results)

checkMemberDecl :: MemberDecl Parsed -> FilePath -> IO [RDF.Diagnostic]
checkMemberDecl methodDecl@(MethodDecl _ _ _ _ (Ident _ ident) _ _ _ _) path =
  if ident `elem` whitelist
    then mzero
    else checkMethodDecl methodDecl path
checkMemberDecl _ _ = mzero

checkMethodDecl :: MemberDecl Parsed -> FilePath -> IO [RDF.Diagnostic]
checkMethodDecl methodDecl path = do
  result <- predictMethodNames methodDecl
  mzero
checkMethodDecl _ _ = mzero
