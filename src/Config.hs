{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types
import Data.List ((\\))

data MethodLimit = MethodLimit
  { checkMethod :: String,
    limitMethod :: String,
    maxInv :: Int
  }

instance FromJSON MethodLimit where
  parseJSON = withObject "MethodLimit" $ \obj -> do
    checkMethod <- obj .: fromString "checkMethod"
    limitMethod <- obj .: fromString "limitMethod"
    maxInv <- obj .: fromString "maxInv"
    checkNoExtraKeys obj [fromString "checkMethod", fromString "limitMethod", fromString "maxInv"]
    pure (MethodLimit checkMethod limitMethod maxInv)

data Rule
  = AvoidMultipleTopLevelDecl
  | AvoidMultipleVarDecl
  | AvoidNegations
  | AvoidOuterNegations
  | AvoidStarImport
  | CheckNonFinalMethodParameters
  | CheckNonPrivateAttributes
  | ConsistentOverrideEqualsHashCode
  | DeclarationOrder
  | DefaultComesLast
  | ExplicitValue
  | InitializeVariables
  | MethodInvNumber [MethodLimit]
  | ModifiedControlVariable
  | MultipleStringLiterals
  | NamingConventions
  | NeedBraces
  | NoCasts {whitelist :: [String]}
  | NoFurtherDataStructures {methodNames :: [String]}
  | NoLoopBreak
  | NoNullPointerExceptionsForControl
  | NoPostIncDecInExpression
  | ParameterNumber {max :: Maybe Int}
  | PreferExpressions
  | ProhibitAnnotations {whitelist :: [String]}
  | ProhibitGermanNames
  | ProhibitMyIdentPrefix
  | ReduceScope
  | RedundantModifiers
  | SameExecutionsInIf
  | UseAssignOp
  | UseElse
  | UseIncrementDecrementOperator
  | UseJavaArrayTypeStyle
  | UsePostIncrementDecrement

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \obj -> do
    rule <- obj .: fromString "rule"
    case rule of
      "AvoidMultipleTopLevelDecl" -> pure AvoidMultipleTopLevelDecl
      "AvoidMultipleVarDecl" -> pure AvoidMultipleVarDecl
      "AvoidNegations" -> pure AvoidNegations
      "AvoidOuterNegations" -> pure AvoidOuterNegations
      "AvoidStarImport" -> pure AvoidStarImport
      "CheckNonFinalMethodParameters" -> pure CheckNonFinalMethodParameters
      "CheckNonPrivateAttributes" -> pure CheckNonPrivateAttributes
      "ConsistentOverrideEqualsHashCode" -> pure ConsistentOverrideEqualsHashCode
      "DeclarationOrder" -> pure DeclarationOrder
      "DefaultComesLast" -> pure DefaultComesLast
      "ExplicitValue" -> pure ExplicitValue
      "InitializeVariables" -> pure InitializeVariables
      "MethodInvNumber" -> parseMethodInvNumber obj
      "ModifiedControlVariable" -> pure ModifiedControlVariable
      "MultipleStringLiterals" -> pure MultipleStringLiterals
      "NamingConventions" -> pure NamingConventions
      "NeedBraces" -> pure NeedBraces
      "NoCasts" -> parseNoCasts obj
      "NoFurtherDataStructures" -> parseNoFurtherDataStructures obj
      "NoLoopBreak" -> pure NoLoopBreak
      "NoNullPointerExceptionsForControl" -> pure NoNullPointerExceptionsForControl
      "NoPostIncDecInExpression" -> pure NoPostIncDecInExpression
      "ParameterNumber" -> parseParameterNumber obj
      "PreferExpressions" -> pure PreferExpressions
      "ProhibitAnnotations" -> parseProhibitAnnotations obj
      "ProhibitGermanNames" -> pure ProhibitGermanNames
      "ProhibitMyIdentPrefix" -> pure ProhibitMyIdentPrefix
      "ReduceScope" -> pure ReduceScope
      "RedundantModifiers" -> pure RedundantModifiers
      "SameExecutionsInIf" -> pure SameExecutionsInIf
      "UseAssignOp" -> pure UseAssignOp
      "UseElse" -> pure UseElse
      "UseIncrementDecrementOperator" -> pure UseIncrementDecrementOperator
      "UseJavaArrayTypeStyle" -> pure UseJavaArrayTypeStyle
      "UsePostIncrementDecrement" -> pure UsePostIncrementDecrement
      _ -> fail ("Unknown Rule: " ++ rule)

parseParameterNumber :: Object -> Parser Rule
parseParameterNumber obj = do
  max <- obj .:! fromString "max"
  checkNoExtraKeys obj [fromString "max"]
  pure (ParameterNumber max)

parseProhibitAnnotations :: Object -> Parser Rule
parseProhibitAnnotations obj = ProhibitAnnotations <$> parseStringList obj "whitelist"

parseNoCasts :: Object -> Parser Rule
parseNoCasts obj = NoCasts <$> parseStringList obj "whitelist"

parseNoFurtherDataStructures :: Object -> Parser Rule
parseNoFurtherDataStructures obj = NoFurtherDataStructures <$> parseStringList obj "methodNames"

parseStringList :: Object -> String -> Parser [String]
parseStringList obj key = do
  strings <- obj .: fromString key
  checkNoExtraKeys obj [fromString key]
  pure strings

parseMethodInvNumber :: Object -> Parser Rule
parseMethodInvNumber obj = do
  limits :: [MethodLimit] <- obj .: fromString "limits"
  checkNoExtraKeys obj [fromString "limits"]
  pure (MethodInvNumber limits)

checkNoExtraKeys :: Object -> [Key] -> Parser ()
checkNoExtraKeys obj allowedKeys = do
  let objKeys = keys obj
  let extraKeys = objKeys \\ (fromString "rule" : allowedKeys)
  unless (null extraKeys) $
    fail $
      "Unexpected keys: " ++ show extraKeys
