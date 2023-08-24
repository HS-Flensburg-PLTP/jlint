module Language.Java.Syntax.Literal (isString, getStringLiteral) where

import Language.Java.Syntax (Literal (..))

isString :: Literal -> Bool
isString (String _ _) = True
isString _ = False

getStringLiteral :: Literal -> String
getStringLiteral (String _ str) = str
getStringLiteral _ = []
