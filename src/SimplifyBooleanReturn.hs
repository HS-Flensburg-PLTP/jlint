{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SimplifyBooleanReturn where


import Data.Generics.Uniplate.Data
import Language.Java.Syntax
import RDF
import CreateDiagnostic


check :: Data.Generics.Uniplate.Data.Biplate β MemberDecl => β -> [Diagnostic]
check x = [ constructDiagnostic n (msg n) | MethodDecl _ _ _ (Ident n) _ _ _ (MethodBody (Just (Block l))) <- Data.Generics.Uniplate.Data.universeBi x, BlockStmt (IfThenElse _ (Return (Just (Lit (Boolean _)))) (Return (Just (Lit (Boolean _))))) <- l]

msg :: String -> String
msg funcName = "IfThenElse with boolean return in function " ++ funcName ++ " can be simplified."
