{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NeedBraces where

import Data.Generics.Uniplate.Data (Biplate, universeBi)
import Language.Java.Syntax
import RDF (simpleDiagnostic, Diagnostic(..))

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = 
    let 
        fBlocks = grabFunc cUnit

        search[] = []
        search ((b,n):xs) = 
            (checkGeneral (grabDoBlockStmt b) n path "A Do-Part") ++ 
            (checkGeneral (grabWhileBlockStmt b) n path "A While-Part") ++ 
            (checkGeneral (grabForBlockStmt b) n path "A For-Part") ++
            (checkGeneral (grabIfThenElseBlockStmt b) n path "A IfThenElse-Part") ++
            (checkGeneral (grabIfThenBlockStmt b) n path "A IfThen-Part") ++
            (checkIfThenElseLast (grabIfThenElseLastBlockStmt b) n path) ++
            search xs
    in
        search fBlocks

grabFunc :: Biplate CompilationUnit MemberDecl => CompilationUnit -> [(MethodBody , String)]
grabFunc cUnit = [(b, n) | MethodDecl _ _ _ (Ident n) _ _ _ b <- universeBi cUnit ]

grabDoBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabDoBlockStmt b = [y | Do y _ <- universeBi b]

grabWhileBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabWhileBlockStmt b = [y | While _ y <- universeBi b]

grabForBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabForBlockStmt b = [y | BasicFor _ _ _ y <- universeBi b]

grabIfThenBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabIfThenBlockStmt b = [y | IfThen _ y <- universeBi b]

grabIfThenElseBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabIfThenElseBlockStmt b = [y | IfThenElse _ y _ <- universeBi b ]

grabIfThenElseLastBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabIfThenElseLastBlockStmt b = [y | IfThenElse _ _ y <- universeBi b ]

msg :: String -> String -> String
msg t funcName = t ++ " in function " ++ funcName ++ " contains no Braces."

checkGeneral :: [Stmt] -> String -> FilePath -> String -> [Diagnostic]
checkGeneral [] _  _ _= []
checkGeneral ((StmtBlock _):xs) funcName  path msgBegin = [] ++ checkGeneral xs funcName path msgBegin
checkGeneral (_:xs) funcName path msgBegin= [simpleDiagnostic (msg msgBegin funcName) path] ++ checkGeneral xs funcName path msgBegin

checkIfThenElseLast :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkIfThenElseLast [] _ _ = []
checkIfThenElseLast ((StmtBlock _) : xs) n path = [] ++ checkIfThenElseLast xs n path
checkIfThenElseLast ((IfThenElse _ _ _) : xs) n path = [] ++ checkIfThenElseLast xs n path
checkIfThenElseLast (_:xs) n  path = [simpleDiagnostic (msg "A IfThenElse-Part" n) path] ++ checkIfThenElseLast xs n path

    
     