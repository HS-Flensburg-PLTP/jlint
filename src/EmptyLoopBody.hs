{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EmptyLoopBody where

import Data.Generics.Uniplate.Data (Biplate, universeBi)
import Language.Java.Syntax
import RDF (simpleDiagnostic, Diagnostic(..))

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = 
    let 
        fBlocks = grabFunc cUnit

        search[] = []
        search ((b,n):xs) = 
            (checkEmptyLoopBody (grabForBlockStmt b) n path "A For-Loop") ++ 
            (checkEmptyLoopBody (grabWhileBlockStmt b) n path "A While-Loop") ++
            (checkEmptyLoopBody (grabDoWhileBlockStmt b) n path "A Do-While-Loop") ++
            search xs
    in
        search fBlocks

grabFunc :: Biplate CompilationUnit MemberDecl => CompilationUnit -> [(MethodBody , String)]
grabFunc cUnit = [(b, n) | MethodDecl _ _ _ (Ident n) _ _ _ b <- universeBi cUnit ]

grabForBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabForBlockStmt b = [y | BasicFor _ _ _ y <- universeBi b]

grabWhileBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabWhileBlockStmt b = [y | While _ y <- universeBi b]

grabDoWhileBlockStmt :: Biplate MethodBody BlockStmt => MethodBody -> [Stmt]
grabDoWhileBlockStmt b = [y | Do y _ <- universeBi b]

msg :: String -> String -> String
msg t funcName = t ++ " in function " ++ funcName ++ " has no Loop-Body."

checkEmptyLoopBody :: [Stmt] -> String -> FilePath -> String -> [Diagnostic]
checkEmptyLoopBody [] _ _ _ = []
checkEmptyLoopBody ((Empty) : xs) funcName path msgBegin = [simpleDiagnostic (msg msgBegin funcName) path] ++ checkEmptyLoopBody xs funcName path msgBegin
checkEmptyLoopBody (_:xs) funcName path msgBegin = [] ++ checkEmptyLoopBody xs funcName path msgBegin
