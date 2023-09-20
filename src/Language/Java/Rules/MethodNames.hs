{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Rules.MethodNames (check) where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (toUpper)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (Located (..), SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import qualified RDF
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (readProcess)

minimumSuggestions :: Int
minimumSuggestions = 3

thresholdSuggestions :: Float
thresholdSuggestions = 0.75

predictMethodNames :: [MethodInfo] -> IO [IdentPrediction]
predictMethodNames methodInfos = do
  maybePath <- lookupEnv "CODE2VEC"
  case maybePath of
    Nothing ->
      -- The application should not crash here
      error "CODE2VEC environment variable not set"
    Just code2VecPath -> do
      writeFile (code2VecPath </> "Input.java") (concatMap (prettyPrint . methodDecl) methodInfos)
      response <-
        readProcess
          (code2VecPath </> ".venv/bin/python3")
          [ code2VecPath </> " code2vec.py",
            "--load",
            code2VecPath </> "models/java14_model/saved_model_iter8.release",
            "--predict"
          ]
          ""
      let jsonData = splitOn "==========" response !! 1
      let predictionSets = decode (pack jsonData) :: Maybe [PredictionSet]
      return
        ( case predictionSets of
            Nothing -> []
            Just pss -> zipWith IdentPrediction pss (map sourceSpan methodInfos)
        )

filterMethodDecl :: [String] -> MemberDecl Parsed -> Maybe MethodInfo
filterMethodDecl whitelist methodDecl@(MethodDecl span _ _ _ ident _ _ _ _)
  | Ident.name ident `notElem` whitelist = return (MethodInfo methodDecl ident span)
  | otherwise = mzero
filterMethodDecl _ _ = mzero

check :: [String] -> CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check whitelist cUnit path = do
  let methodInfos = mapMaybe (filterMethodDecl whitelist) (universeBi cUnit)
  predictionSets <- predictMethodNames methodInfos
  let filteredPredictionSets =
        filter
          ( \(IdentPrediction (PredictionSet originalName predictions) _) ->
              all (\(Prediction predictedName _) -> originalName /= predictedName) predictions
          )
          predictionSets
  let filteredPredictionSets' = map accumulatedProbability filteredPredictionSets
  return (map (message path) filteredPredictionSets')

message :: FilePath -> IdentPrediction -> RDF.Diagnostic
message path (IdentPrediction (PredictionSet originalName predictions) span) =
  RDF.rangeDiagnostic
    "Language.Java.Rules.PredictMethodNames"
    ( [ "Der Name",
        Markdown.code (toCamelCase originalName),
        "ist schlecht gewählt.",
        "Folgende Vorschläge eignen sich vielleicht besser:"
      ]
        ++ map (Markdown.code . pretty) predictions
    )
    span
    path

-- Helping data structures

data MethodInfo = MethodInfo (MemberDecl Parsed) Ident SourceSpan

instance Located MethodInfo where
  sourceSpan (MethodInfo _ _ span) = span

methodDecl :: MethodInfo -> MemberDecl Parsed
methodDecl (MethodInfo memberDecl _ _) = memberDecl

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

pretty :: Prediction -> String
pretty (Prediction name _) = toCamelCase name

toCamelCase :: [String] -> String
toCamelCase [] = ""
toCamelCase (part : parts) = part ++ concatMap firstToUpper parts

firstToUpper :: String -> String
firstToUpper "" = ""
firstToUpper (c : cs) = toUpper c : cs

instance FromJSON Prediction

instance FromJSON PredictionSet

data IdentPrediction = IdentPrediction PredictionSet SourceSpan

instance Located IdentPrediction where
  sourceSpan (IdentPrediction _ span) = span

accumulatedProbability :: IdentPrediction -> IdentPrediction
accumulatedProbability (IdentPrediction (PredictionSet originalName predictions) span) =
  IdentPrediction (PredictionSet {originalName = originalName, predictions = filterPredictions predictions}) span

filterPredictions :: [Prediction] -> [Prediction]
filterPredictions preds = filterPredictions' preds 0 0.0
  where
    filterPredictions' :: [Prediction] -> Int -> Float -> [Prediction]
    filterPredictions' [] _ _ = []
    filterPredictions' (p : ps) count accumulatedWeight =
      if accumulatedWeight >= thresholdSuggestions && count >= minimumSuggestions
        then []
        else p : filterPredictions' ps (count + 1) (accumulatedWeight + weight p)
