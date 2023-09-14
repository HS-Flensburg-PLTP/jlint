module Language.Java.Rules.AvoidNegations (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkStatement cUnit path ++ checkExpression cUnit path

checkStatement :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkStatement cUnit path = do
  stmt <- universeBi cUnit
  checkIfThenElse stmt
  where
    checkIfThenElse :: Stmt Parsed -> [RDF.Diagnostic]
    checkIfThenElse (IfThenElse span cond _ _) =
      if isNegation cond
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidNegations"
                [ "Wenn möglich sollte man Negationen vermeiden, da sie schwer zu verstehen sind. Bei einer",
                  Markdown.code "if-then-else" ++ "-Anweisung",
                  "kann man die Negation vermeiden, indem man die Zweige des Konditionals tauscht."
                ]
                span
                path
            )
        else mzero
    checkIfThenElse _ = mzero

checkExpression :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkExpression cUnit path = do
  stmt <- universeBi cUnit
  checkCond stmt
  where
    checkCond :: Exp Parsed -> [RDF.Diagnostic]
    checkCond (Cond span cond _ _) =
      if isNegation cond
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.AvoidNegations"
                ["Wenn möglich sollte man Negationen vermeiden, da sie schwer zu verstehen sind. Bei einem ternären Operator kann man die Negation vermeiden, indem man die Zweige des Operators tauscht."]
                span
                path
            )
        else mzero
    checkCond _ = mzero

isNegation :: Exp Parsed -> Bool
isNegation (PreNot _ _) = True
isNegation (BinOp _ _ NotEq _) = True
isNegation _ = False
