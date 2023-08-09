{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.NoLoopBreak (check) where

import Control.Monad.Reader
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified RDF

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
  stmt <- listDifference (universeBi stmt) (checkLoops stmt)
  filterReturn stmt
  where
    filterReturn (Return span _) = returnMsg path span
    filterReturn _ = mzero

checkBreak :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkBreak stmt path = do
  stmt <- listDifference (universeBi stmt) (checkSwitchBlock stmt ++ checkLoops stmt)
  filterBreak stmt
  where
    filterBreak (Break span _) = breakMsg path span
    filterBreak _ = mzero

checkLoops :: Stmt Parsed -> [Stmt Parsed]
checkLoops stmt = do
  loop <- universeBi stmt
  maybe mzero universeBi (extractLoopBody loop)

extractLoopBody :: Stmt Parsed -> Maybe (Stmt Parsed)
extractLoopBody (While _ _ stmt) = Just stmt
extractLoopBody (Do _ stmt _) = Just stmt
extractLoopBody (BasicFor _ _ _ _ stmt) = Just stmt
extractLoopBody (EnhancedFor _ _ _ _ _ stmt) = Just stmt
extractLoopBody _ = Nothing

checkSwitchBlock :: Stmt Parsed -> [Stmt Parsed]
checkSwitchBlock stmt = do
  Switch _ _ blocks :: Stmt Parsed <- universeBi stmt
  universeBi blocks

listDifference :: [Stmt Parsed] -> [Stmt Parsed] -> [Stmt Parsed]
listDifference completeList excludedList = do
  elem <- completeList
  if any (eq IncludeSourceSpan elem) excludedList
    then mzero
    else return elem

breakMsg :: FilePath -> SourceSpan -> [RDF.Diagnostic]
breakMsg path span = return (RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Eine Schleife sollte nicht mit einem Break beendet werden." span path)

returnMsg :: FilePath -> SourceSpan -> [RDF.Diagnostic]
returnMsg path span = return (RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Eine Schleife sollte nicht mit einem Return beendet werden." span path)
