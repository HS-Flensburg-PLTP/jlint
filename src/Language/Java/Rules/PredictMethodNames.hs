{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Java.Rules.PredictMethodNames where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import Data.Char (toLower, toUpper)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Split
import GHC.Generics (Generic)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process

minimumSuggestions :: Int
minimumSuggestions = 3

thresholdSuggestions :: Float
thresholdSuggestions = 0.75

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
  maybePath <- lookupEnv "CODE2VEC"
  case maybePath of
    Nothing ->
      -- The application should not crash here
      error "CODE2VEC environment variable not set"
    Just code2VecPath -> do
      writeFile (code2VecPath </> "Input.java") (concatMap prettyPrint methodDecls)
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

check :: [String] -> CompilationUnit Parsed -> FilePath -> IO [RDF.Diagnostic]
check whitelist cUnit path = do
  let methodDecls = concatMap (checkMethodDecl whitelist) (universeBi cUnit)
  if null methodDecls
    then return []
    else do
      predictionSets <- predictMethodNames methodDecls
      let filteredPredictionSets =
            filter
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
      let filteredPredictionSets' = checkPredictionSets filteredPredictionSets
      return
        ( map
            ( \(PredictionSet originalName predictions, sourceSpan) ->
                RDF.rangeDiagnostic
                  "Language.Java.Rules.PredictMethodNames"
                  ( "Der Name "
                      ++ Markdown.code (toCamelCase originalName)
                      ++ " ist schlecht gewählt. Folgende Vorschläge eignen sich vielleicht besser: "
                      ++ unwords
                        ( map
                            ( \(Prediction name _) ->
                                Markdown.code (toCamelCase name)
                            )
                            predictions
                        )
                  )
                  sourceSpan
                  path
            )
            filteredPredictionSets'
        )

toCamelCase :: [String] -> String
toCamelCase (x : xs) =
  x
    ++ concatMap
      ( \(n : ns) ->
          toUpper n : map toLower ns
      )
      xs
toCamelCase [] = []

checkMethodDecl :: [String] -> MemberDecl Parsed -> [MemberDecl Parsed]
checkMethodDecl whitelist methodDecl@(MethodDecl _ _ _ _ (Ident _ ident) _ _ _ _) =
  [methodDecl | ident `notElem` whitelist]
checkMethodDecl _ _ = []

checkPredictionSets :: [(PredictionSet, SourceSpan)] -> [(PredictionSet, SourceSpan)]
checkPredictionSets =
  map
    ( \(PredictionSet originalName predictions, sourceSpan) ->
        ( PredictionSet
            { originalName = originalName,
              predictions = calculatePredictions predictions
            },
          sourceSpan
        )
    )

calculatePredictions :: [Prediction] -> [Prediction]
calculatePredictions preds = calculatePredictions' preds [] 0 0.0
  where
    calculatePredictions' :: [Prediction] -> [Prediction] -> Int -> Float -> [Prediction]
    calculatePredictions' [] akk _ _ = akk
    calculatePredictions' (x@(Prediction _ weight) : xs) akk count accumulatedWeight =
      if accumulatedWeight >= thresholdSuggestions
        then
          if count < minimumSuggestions
            then calculatePredictions' xs (akk ++ [x]) (count + 1) (accumulatedWeight + weight)
            else akk
        else calculatePredictions' xs (akk ++ [x]) (count + 1) (accumulatedWeight + weight)
