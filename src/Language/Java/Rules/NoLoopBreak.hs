{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.NoLoopBreak (check) where

import Control.Monad.Reader
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

{-
check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkStatements methods path

checkStatements :: (String, MethodBody Parsed) -> FilePath -> [RDF.Diagnostic]
checkStatements (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  runReader (checkStatement stmt) (methodName, path)

checkStatement :: Stmt Parsed -> Reader (String, FilePath) [RDF.Diagnostic]
checkStatement (While _ _ stmt) = checkLoop stmt
checkStatement (Do _ stmt _) = checkLoop stmt
checkStatement (BasicFor _ _ _ _ stmt) = checkLoop stmt
checkStatement (EnhancedFor _ _ _ _ _ stmt) = checkLoop stmt
checkStatement _ = return []

checkLoop :: Stmt Parsed -> Reader (String, FilePath) [RDF.Diagnostic]
checkLoop (StmtBlock (Block block)) = extractLoopBody block
checkLoop (IfThen _ _ stmt) = checkLoop stmt
checkLoop (IfThenElse _ _ stmt1 stmt2) = do
  stm1 <- checkLoop stmt1
  stm2 <- checkLoop stmt2
  return (stm1 ++ stm2)
checkLoop (Return _ _) = do
  (_, path) <- ask
  return [RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Exit Loop with return" dummySourceSpan path]
checkLoop (Break _ _) = do
  (_, path) <- ask
  return [RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Exit Loop with break" dummySourceSpan path]
checkLoop stmt = checkStatement stmt

extractLoopBody :: [BlockStmt Parsed] -> Reader (String, FilePath) [RDF.Diagnostic]
extractLoopBody ((BlockStmt _ block) : xs) = do
  stmtblock <- checkLoop block
  elb <- extractLoopBody xs
  return (stmtblock ++ elb)
extractLoopBody ((LocalClass _) : xs) = extractLoopBody xs
extractLoopBody ((LocalVars {}) : xs) = extractLoopBody xs
extractLoopBody [] = return []
-}

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStatement stmt path

checkStatement :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkStatement (While _ _ stmt) path = checkReturn stmt path ++ checkBreak stmt path
checkStatement (Do _ stmt _) path = checkReturn stmt path ++ checkBreak stmt path
checkStatement (BasicFor _ _ _ _ stmt) path = checkReturn stmt path ++ checkBreak stmt path
checkStatement (EnhancedFor _ _ _ _ _ stmt) path = checkReturn stmt path ++ checkBreak stmt path
checkStatement _ _ = mzero

checkReturn :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkReturn stmt path = do
  let allReturns :: [Stmt Parsed] = universeBi stmt
  stmt <- listDifference allReturns (checkLoops stmt)
  returnMsg path stmt

checkBreak :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkBreak stmt path = do
  let allBreaks :: [Stmt Parsed] = universeBi stmt
  stmt <- listDifference allBreaks (checkSwitchBlock stmt ++ checkLoops stmt)
  breakMsg path stmt

checkLoops :: Stmt Parsed -> [Stmt Parsed]
checkLoops stmt = do
  loop <- universeBi stmt
  loopBody <- extractLoopBody loop
  universeBi loopBody

extractLoopBody :: Stmt Parsed -> [Stmt Parsed]
extractLoopBody (While _ _ stmt) = return stmt
extractLoopBody (Do _ stmt _) = return stmt
extractLoopBody (BasicFor _ _ _ _ stmt) = return stmt
extractLoopBody (EnhancedFor _ _ _ _ _ stmt) = return stmt
extractLoopBody _ = mzero

checkSwitchBlock :: Stmt Parsed -> [Stmt Parsed]
checkSwitchBlock stmt = do
  Switch _ _ blocks :: Stmt Parsed <- universeBi stmt
  let switchBreaks :: [Stmt Parsed] = universeBi blocks
  switchBreaks

listDifference :: [Stmt Parsed] -> [Stmt Parsed] -> [Stmt Parsed]
listDifference completeList excludedList = do
  elem <- completeList
  if any (eq IncludeSourceSpan elem) excludedList
    then mzero
    else return elem

breakMsg :: FilePath -> Stmt Parsed -> [RDF.Diagnostic]
breakMsg path (Break span _) = return (RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Eine Schleife sollte nicht mit einem Break beendet werden." span path)
breakMsg _ _ = mzero

returnMsg :: FilePath -> Stmt Parsed -> [RDF.Diagnostic]
returnMsg path (Return span _) = return (RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Eine Schleife sollte nicht mit einem Return beendet werden." span path)
returnMsg _ _ = mzero
