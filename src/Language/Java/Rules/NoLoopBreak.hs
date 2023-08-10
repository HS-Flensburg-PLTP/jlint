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
  case extractLoopBody stmt of
    Just loopBody -> checkReturn loopBody path ++ checkBreak loopBody path
    Nothing -> mzero

checkReturn :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkReturn stmt path = do
  (Return span _) <- universeBi stmt // checkLoops stmt
  returnMsg path span

checkBreak :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkBreak stmt path = do
  (Break span _) <- universeBi stmt // (checkLoops stmt ++ checkSwitchBlock stmt)
  breakMsg path span

checkLoops :: Stmt Parsed -> [Stmt Parsed]
checkLoops stmt = do
  loop <- universeBi stmt
  maybe mzero universeBi (extractLoopBody loop)

checkSwitchBlock :: Stmt Parsed -> [Stmt Parsed]
checkSwitchBlock stmt = do
  Switch _ _ _ blocks :: Stmt Parsed <- universeBi stmt
  let switchBreaks :: [Stmt Parsed] = universeBi blocks
  switchBreaks

extractLoopBody :: Stmt Parsed -> Maybe (Stmt Parsed)
extractLoopBody (While _ _ stmt) = Just stmt
extractLoopBody (Do _ stmt _) = Just stmt
extractLoopBody (BasicFor _ _ _ _ stmt) = Just stmt
extractLoopBody (EnhancedFor _ _ _ _ _ stmt) = Just stmt
extractLoopBody _ = Nothing

(//) :: [Stmt Parsed] -> [Stmt Parsed] -> [Stmt Parsed]
completeList // excludedList = do
  elem <- completeList
  if any (eq IncludeSourceSpan elem) excludedList
    then mzero
    else return elem

breakMsg :: FilePath -> SourceSpan -> [RDF.Diagnostic]
breakMsg path span = return (RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Eine Schleife sollte nicht mit einem Break beendet werden." span path)

returnMsg :: FilePath -> SourceSpan -> [RDF.Diagnostic]
returnMsg path span = return (RDF.rangeDiagnostic "Language.Java.Rules.NoLoopBreak" "Eine Schleife sollte nicht mit einem Return beendet werden." span path)
