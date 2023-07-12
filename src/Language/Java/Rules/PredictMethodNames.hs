{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.PredictMethodNames where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Split
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF
import System.Process

whitelist :: [String]
whitelist = [""]

data PredictionSet
  = PredictionSet String [Prediction]

data Prediction
  = Prediction String Float

predictMethodNames :: [MemberDecl Parsed] -> IO [PredictionSet]
predictMethodNames methodDecls = do
  response <-
    readProcess
      "/home/averodas/Schreibtisch/haskell/code2seqFork/.venv/bin/python3.6"
      [ "/home/averodas/Schreibtisch/haskell/code2seqFork/code2seq.py",
        "--load",
        "/home/averodas/Schreibtisch/haskell/code2seqFork/models/java-large-model/model_iter52.release",
        "--predict",
        "--code",
        concatMap prettyPrint methodDecls
      ]
      ""
  let responses = splitOn "Predicted: " response
  putStrLn (concatMap (\s -> show ("HALLO" ++ s)) responses)
  mzero

check :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  let methodDecls = concatMap checkMethodDecl (universeBi cUnit)
  response <- predictMethodNames methodDecls
  return []

checkMethodDecl :: MemberDecl Parsed -> [MemberDecl Parsed]
checkMethodDecl methodDecl@(MethodDecl _ _ _ _ (Ident _ ident) _ _ _ _) =
  [methodDecl | ident `notElem` whitelist]
checkMethodDecl _ = []

{-
.venv/bin/python3.6 code2seq.py --load models/java-large-model/model_iter52.release --predict --code 'public String getName() { return name; } public String getAnotherName() { return name; }'
-}
