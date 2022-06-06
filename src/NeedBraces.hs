{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module NeedBraces where

import Data.Generics.Uniplate.Data
import Language.Java.Syntax
import RDF
import CreateDiagnostic


check :: Data.Generics.Uniplate.Data.Biplate β MemberDecl => β -> [Diagnostic]
check x = [ constructDiagnostic n (msg n) | MethodDecl _ _ _ (Ident n) _ _ _ (MethodBody (Just (Block l))) <- Data.Generics.Uniplate.Data.universeBi x, BlockStmt (IfThen _ (Return _)) <- l]

msg :: String -> String
msg funcName = "IfThen in function " ++ funcName ++ " contains no Braces."
