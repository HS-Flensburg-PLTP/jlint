module Language.Java.Rules.AvoidNegations (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkStatement cUnit path ++ checkExpression cUnit path

checkStatement :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkStatement cUnit path = do
  stmt <- universeBi cUnit
  checkIfThenElse stmt path
  where
    checkIfThenElse (IfThenElse span cond _ _) path =
      if isNegation cond
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidNegations"
                "Wenn möglich sollte man Negationen vermeiden, da sie schwer zu verstehen sind. Bei einer `if-then-else`-Anweisung kann man die Negation vermeiden, indem man die Zweige des Konditionals tauscht."
                span
                path
            )
        else mzero
    checkIfThenElse _ _ = mzero

checkExpression :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkExpression cUnit path = do
  stmt <- universeBi cUnit
  checkCond stmt path
  where
    checkCond (Cond span cond _ _) path =
      if isNegation cond
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidNegations"
                "Wenn möglich sollte man Negationen vermeiden, da sie schwer zu verstehen sind. Bei einem ternären Operator kann man die Negation vermeiden, indem man die Zweige des Operators tauscht."
                span
                path
            )
        else mzero
    checkCond _ _ = mzero

isNegation :: Exp Parsed -> Bool
isNegation (PreNot _) = True
isNegation (BinOp _ NotEq _) = True
isNegation _ = False
