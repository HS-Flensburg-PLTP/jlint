{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NeedBraces where

import Data.Generics.Uniplate.Data (Biplate, universeBi)
import Language.Java.Syntax
import RDF (simpleDiagnostic, Diagnostic(..))

check :: CompilationUnit -> [Diagnostic]
check cUnit = 
    let 
        fBlocks = grabFunc cUnit

        search[] = []
        search ((b,n):xs) = 
            (checkDoWhile (grabDoBlockStmt b) n) ++ 
            (checkWhile (grabWhileBlockStmt b) n) ++ 
            (checkFor (grabForBlockStmt b) n) ++
            (checkIfThenElse (grabIfThenElseBlockStmt b) n) ++
            (checkifThen (grabIfThenBlockStmt b) n) ++
            (checkIfThenElseLast (grabIfThenElseLastBlockStmt b) n) ++
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

msg :: String -> String
msg funcName = "IfThen in function " ++ funcName ++ " contains no Braces."

checkDoWhile [] n = []
checkDoWhile ((StmtBlock _):xs) n = [] ++ checkDoWhile xs n
checkDoWhile (_:xs) n = [simpleDiagnostic "NeedBraces Function " (msg n) " a Do-Part need Braces"] ++ checkDoWhile xs n

checkWhile [] n = []
checkWhile ((StmtBlock _) : xs) n = [] ++ checkWhile xs n
checkWhile (_:xs) n = [simpleDiagnostic "NeedBraces Function " (msg n) " a While-Part need Braces"] ++ checkWhile xs n


checkFor [] n = []
checkFor ((StmtBlock _) : xs) n = [] ++ checkFor xs n
checkFor (_:xs) n = [simpleDiagnostic "NeedBraces Function " (msg n) " a For-Part need Braces"] ++ checkFor xs n

checkIfThenElse [] n = []
checkIfThenElse ((StmtBlock _) : xs) n = [] ++ checkIfThenElse xs n
checkIfThenElse (_:xs) n = [simpleDiagnostic "NeedBraces Function " (msg n) " a IfThenElse-Part need Braces"] ++ checkIfThenElse xs n

checkifThen [] n = []
checkifThen ((StmtBlock _) : xs) n = [] ++ checkifThen xs n
checkifThen (_:xs) n = [simpleDiagnostic "NeedBraces Function " (msg n) " a IfThen-Part need Braces"] ++ checkifThen xs n

checkIfThenElseLast [] n = []
checkIfThenElseLast ((StmtBlock _) : xs) n = [] ++ checkIfThenElseLast xs n
checkIfThenElseLast ((IfThenElse _ _ _) : xs) n = [] ++ checkIfThenElseLast xs n
checkIfThenElseLast (_:xs) n =  [simpleDiagnostic "NeedBraces Function " (msg n) " a IfThenElse-Last-Part need Braces"] ++ checkIfThenElse xs n

    
     