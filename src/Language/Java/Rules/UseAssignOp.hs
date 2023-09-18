module Language.Java.Rules.UseAssignOp (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkStmt (universeBi cUnit)
  where
    checkStmt :: Stmt Parsed -> Maybe RDF.Diagnostic
    checkStmt (ExpStmt span (Assign _ (NameLhs name1) EqualA (BinOp _ (ExpName name2) op _))) =
      if eq IgnoreSourceSpan name1 name2
        then fmap (message path span op) (equalVariant op)
        else mzero
    checkStmt _ = mzero

equalVariant :: Op -> Maybe AssignOp
equalVariant Mult = Just MultA
equalVariant Div = Just DivA
equalVariant Rem = Just RemA
equalVariant Add = Just AddA
equalVariant Sub = Just SubA
equalVariant LShift = Just LShiftA
equalVariant RShift = Just RShiftA
equalVariant RRShift = Just RRShiftA
equalVariant And = Just AndA
equalVariant Or = Just OrA
equalVariant Xor = Just XorA
equalVariant _ = Nothing

message :: FilePath -> SourceSpan -> Op -> AssignOp -> RDF.Diagnostic
message path span op assignOp =
  RDF.rangeDiagnostic
    "Language.Java.Rules.UseAssignOp"
    [ "Anstelle einer Anweisung",
      Markdown.code ("x = x " ++ prettyPrint op ++ " exp"),
      "sollte",
      Markdown.code ("x " ++ prettyPrint assignOp ++ " exp"),
      "verwendet werden."
    ]
    span
    path
