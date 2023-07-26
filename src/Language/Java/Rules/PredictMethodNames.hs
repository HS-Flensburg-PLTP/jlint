{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Rules.PredictMethodNames where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Split
import GHC.Generics (Generic)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF
import System.Process

whitelist :: [String]
whitelist = [""]

data PredictionSet = PredictionSet
  { originalName :: [String],
    predictions :: [Prediction]
  }
  deriving (Generic, Show)

data Prediction = Prediction
  { predictedName :: [String],
    weight :: Float
  }
  deriving (Generic, Show)

instance FromJSON Prediction

instance FromJSON PredictionSet

predictMethodNames :: [MemberDecl Parsed] -> IO [(PredictionSet, SourceSpan)]
predictMethodNames methodDecls = do
  writeFile "../code2vec/Input.java" (concatMap prettyPrint methodDecls)
  response <-
    readProcess
      "../code2vec/.venv/bin/python3.6"
      [ "../code2vec/code2vec.py",
        "--load",
        "../code2vec/models/java14_model/saved_model_iter8.release",
        "--predict"
      ]
      ""
  let jsonData = splitOn "==========" response !! 1
  let predictionSets = decode (C.pack jsonData) :: Maybe [PredictionSet]
  case predictionSets of
    Nothing -> return []
    Just pss ->
      return
        ( zipWith
            (\((MethodDecl _ _ _ _ (Ident sourceSpan _) _ _ _ _)) predictionSet -> (predictionSet, sourceSpan))
            methodDecls
            pss
        )

check :: CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check cUnit path = do
  let methodDecls = concatMap checkMethodDecl (universeBi cUnit)
  predictionSets <- predictMethodNames methodDecls
  return
    ( map
        ( \(PredictionSet originalName _, sourceSpan) ->
            RDF.rangeDiagnostic
              "Language.Java.Rules.PredictMethodNames"
              ("Der Name " ++ Markdown.code (unwords originalName) ++ " ist schlecht gewählt. Folgende Vorschläge eignen sich vielleicht besser: ")
              sourceSpan
              path
        )
        ( filter
            ( \(PredictionSet originalName predictions, _) ->
                not
                  ( any
                      ( \(Prediction predictedName _) ->
                          originalName == predictedName
                      )
                      predictions
                  )
            )
            predictionSets
        )
    )

checkMethodDecl :: MemberDecl Parsed -> [MemberDecl Parsed]
checkMethodDecl methodDecl@(MethodDecl _ _ _ _ (Ident _ ident) _ _ _ _) =
  [methodDecl | ident `notElem` whitelist]
checkMethodDecl _ = []

{-
.venv/bin/python3.6 code2seq.py --load models/java-large-model/model_iter52.release --predict --code 'public String getName() { return name; } public String getAnotherName() { return name; }'
-}
