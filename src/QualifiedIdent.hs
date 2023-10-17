{-# LANGUAGE OverloadedStrings #-}

module QualifiedIdent
  ( QualifiedIdent (..),
    hasClassName,
  )
where

import Data.Text (splitOn, unpack)
import Data.Yaml (FromJSON (..), Value (..))
import Language.Java.Syntax (Ident)
import qualified Language.Java.Syntax.Ident as Ident

data QualifiedIdent = QualifiedIdent {className :: String, methodName :: String}

instance FromJSON QualifiedIdent where
  parseJSON (String text) =
    case splitOn "." text of
      [className, methodName] -> pure (QualifiedIdent (unpack className) (unpack methodName))
      _ -> fail "MethodName should have the form class.name"
  parseJSON _ = fail "MethodName should be a string"

hasClassName :: Ident -> QualifiedIdent -> Bool
hasClassName ident qIdent = className qIdent == Ident.name ident
