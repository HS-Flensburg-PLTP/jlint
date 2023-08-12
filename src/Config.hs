module Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types
import Data.List ((\\))

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
  | MethodInvNumber {called :: String, limited :: String, maxInv :: Int}
  | ModifiedControlVariable
  | NeedBraces
  | NoCasts {whitelist :: [String]}
  | NoFurtherDataStructures {methodNames :: [String]}
  | NoLoopBreak
  | NoNullPointerExceptionsForControl
  | NoPostIncDecInExpression
  | ParameterNumber {max :: Maybe Int}
  | PreferExpressions
  | ProhibitAnnotations {whitelist :: [String]}
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
      "NeedBraces" -> pure NeedBraces
      "NoCasts" -> parseNoCasts obj
      "NoFurtherDataStructures" -> parseNoFurtherDataStructures obj
      "NoLoopBreak" -> pure NoLoopBreak
      "NoNullPointerExceptionsForControl" -> pure NoNullPointerExceptionsForControl
      "NoPostIncDecInExpression" -> pure NoPostIncDecInExpression
      "ParameterNumber" -> parseParameterNumber obj
      "PreferExpressions" -> pure PreferExpressions
      "ProhibitAnnotations" -> parseProhibitAnnotations obj
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
  called <- obj .: fromString "called"
  limited <- obj .: fromString "limited"
  maxInv <- obj .: fromString "maxInv"
  checkNoExtraKeys obj [fromString "called", fromString "limited", fromString "maxInv"]
  pure (MethodInvNumber called limited maxInv)

checkNoExtraKeys :: Object -> [Key] -> Parser ()
checkNoExtraKeys obj allowedKeys = do
  let objKeys = keys obj
  let extraKeys = objKeys \\ (fromString "rule" : allowedKeys)
  unless (null extraKeys) $
    fail $
      "Unexpected keys: " ++ show extraKeys
