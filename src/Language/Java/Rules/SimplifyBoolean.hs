module Language.Java.Rules.SimplifyBoolean (check) where

import Control.Monad (mplus, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkStmt (universeBi cUnit) `mplus` mapMaybe checkExp (universeBi cUnit)
  where
    checkStmt :: Stmt Parsed -> Maybe RDF.Diagnostic
    checkStmt (IfThenElse span cond thenStmt elseStmt)
      | isBoolLiteralReturn True thenStmt && isBoolLiteralReturn False elseStmt =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                [ "Falls der Ausdruck",
                  Markdown.code (prettyPrint cond),
                  "den Wert",
                  Markdown.code "true",
                  "hat, liefert die gesamte",
                  Markdown.code "if" ++ "-Anweisung",
                  "den Wert",
                  Markdown.code "true" ++ ".",
                  "Falls der Ausdruck",
                  Markdown.code (prettyPrint cond),
                  "den Wert",
                  Markdown.code "false",
                  "hat, liefert die gesamte",
                  Markdown.code "if" ++ "-Anweisung",
                  "den Wert",
                  Markdown.code "false" ++ ".",
                  "Daher kann die",
                  Markdown.code "if" ++ "-Anweisung",
                  "zur Bedingung vereinfacht werden."
                ]
                span
                path
            )
      | isBoolLiteralReturn False thenStmt && isBoolLiteralReturn True elseStmt =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                ["Diese Anweisung kann durch eine Negation ersetzt werden."]
                span
                path
            )
      | otherwise = mzero
    checkStmt _ = mzero

    checkExp :: Exp Parsed -> Maybe RDF.Diagnostic
    checkExp (Cond span cond thenExp elseExp)
      | isBoolLiteral True thenExp && isBoolLiteral False elseExp =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                [ "Falls der Ausdruck",
                  Markdown.code (prettyPrint cond),
                  "den Wert",
                  Markdown.code "true",
                  "hat, liefert der gesamte bedingte Ausdruck den Wert",
                  Markdown.code "true" ++ ".",
                  "Falls der Ausdruck",
                  Markdown.code (prettyPrint cond),
                  "den Wert",
                  Markdown.code "false",
                  "hat, liefert der gesamte bedingte Ausdruck den Wert",
                  Markdown.code "false" ++ ".",
                  "Daher kann der gesamte bedingte Ausdruck durch die Bedingung ersetzt werden."
                ]
                span
                path
            )
      | isBoolLiteral False thenExp && isBoolLiteral True elseExp =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                ["Dieser Ausdruck kann durch eine Negation ersetzt werden."]
                span
                path
            )
      | otherwise = mzero
    checkExp (BinOp span leftExp Equal rightExp)
      | isBoolLiteral True leftExp || isBoolLiteral True rightExp =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                [ "Der Vergleich mit",
                  Markdown.code "true",
                  "ist unnötig und sollte entfernt werden."
                ]
                span
                path
            )
      | isBoolLiteral False leftExp || isBoolLiteral False rightExp =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                [ "Statt mit",
                  Markdown.code "false",
                  "zu vergleichen sollte die Negation verwendet werden."
                ]
                span
                path
            )
      | otherwise = mzero
    checkExp (BinOp span leftExp NotEq rightExp)
      | isBoolLiteral True leftExp || isBoolLiteral True rightExp =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                [ "Statt mit",
                  Markdown.code "true",
                  "zu vergleichen sollte die Negation verwendet werden."
                ]
                span
                path
            )
      | isBoolLiteral False leftExp || isBoolLiteral False rightExp =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SimplifyBoolean"
                [ "Der Vergleich mit",
                  Markdown.code "false",
                  "ist unnötig und sollte entfernt werden."
                ]
                span
                path
            )
      | otherwise = mzero
    checkExp _ = mzero

isBoolLiteralReturn :: Bool -> Stmt Parsed -> Bool
isBoolLiteralReturn value (Return _ (Just exp)) = isBoolLiteral value exp
isBoolLiteralReturn value (StmtBlock (Block _ [BlockStmt stmt])) = isBoolLiteralReturn value stmt
isBoolLiteralReturn _ _ = False

isBoolLiteral :: Bool -> Exp Parsed -> Bool
isBoolLiteral value (Lit (Boolean _ litValue)) = value == litValue
isBoolLiteral _ _ = False
