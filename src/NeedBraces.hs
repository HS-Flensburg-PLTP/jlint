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
            (checkDoWhile (grabDoBlockStmt b) n path) ++ 
            (checkWhile (grabWhileBlockStmt b) n path) ++ 
            (checkFor (grabForBlockStmt b) n path) ++
            (checkIfThenElse (grabIfThenElseBlockStmt b) n path) ++
            (checkifThen (grabIfThenBlockStmt b) n path) ++
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

checkDoWhile :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkDoWhile [] _  _= []
checkDoWhile ((StmtBlock _):xs) n  path= [] ++ checkDoWhile xs n path
checkDoWhile (_:xs) n  path= [simpleDiagnostic (msg "A Do-Part" n) path] ++ checkDoWhile xs n path

checkWhile :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkWhile [] _  _= []
checkWhile ((StmtBlock _) : xs) n  path= [] ++ checkWhile xs n path
checkWhile (_:xs) n  path= [simpleDiagnostic (msg "A While-Part" n) path] ++ checkWhile xs n path

checkFor :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkFor [] _  _= []
checkFor ((StmtBlock _) : xs) n  path= [] ++ checkFor xs n path
checkFor (_:xs) n  path= [simpleDiagnostic (msg "A For-Part" n) path] ++ checkFor xs n path

checkIfThenElse :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkIfThenElse [] _  _= []
checkIfThenElse ((StmtBlock _) : xs) n path = [] ++ checkIfThenElse xs n path
checkIfThenElse (_:xs) n path = [simpleDiagnostic (msg "A IfThenElse-Part" n) path] ++ checkIfThenElse xs n path

checkifThen :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkifThen [] _ _ = []
checkifThen ((StmtBlock _) : xs) n path = [] ++ checkifThen xs n path
checkifThen (_:xs) n path = [simpleDiagnostic (msg "A IfThen-Part" n) path] ++ checkifThen xs n path

checkIfThenElseLast :: [Stmt] -> String -> FilePath -> [Diagnostic]
checkIfThenElseLast [] _ _ = []
checkIfThenElseLast ((StmtBlock _) : xs) n path = [] ++ checkIfThenElseLast xs n path
checkIfThenElseLast ((IfThenElse _ _ _) : xs) n path = [] ++ checkIfThenElseLast xs n path
checkIfThenElseLast (_:xs) n  path=  [simpleDiagnostic (msg "A IfThenElse-Part" n) path] ++ checkIfThenElse xs n path

    
     