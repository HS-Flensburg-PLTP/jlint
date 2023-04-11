module Language.Java.Rules.UseAssignOp (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( AssignOp (..),
    CompilationUnit,
    Exp (Assign, BinOp, ExpName),
    Lhs (NameLhs),
    Op (..),
    Stmt (ExpStmt),
  )
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (ExpStmt range (Assign _ (NameLhs name1) EqualA (BinOp (ExpName name2) op _))) =
      if name1 == name2
        then case equalVariant op of
          Just assignOp ->
            return
              (RDF.rangeDiagnostic "Language.Java.Rules.UseAssignOp" (message op assignOp) range path)
          Nothing -> mzero
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

prettyOp :: Op -> String
prettyOp Mult = "*"
prettyOp Div = "/"
prettyOp Rem = "%"
prettyOp Add = "+"
prettyOp Sub = "/"
prettyOp LShift = "<"
prettyOp RShift = ">"
prettyOp RRShift = ">>"
prettyOp And = "&"
prettyOp Or = "|"
prettyOp Xor = "^"
prettyOp LThan = "<"
prettyOp GThan = ">"
prettyOp LThanE = "<="
prettyOp GThanE = ">="
prettyOp Equal = "="
prettyOp NotEq = "!="
prettyOp CAnd = "&&"
prettyOp COr = "||"

prettyAssignOp :: AssignOp -> String
prettyAssignOp EqualA = "="
prettyAssignOp MultA = "*="
prettyAssignOp DivA = "/="
prettyAssignOp RemA = "%="
prettyAssignOp AddA = "+="
prettyAssignOp SubA = "-="
prettyAssignOp LShiftA = "<<="
prettyAssignOp RShiftA = ">>="
prettyAssignOp RRShiftA = ">>>="
prettyAssignOp AndA = "&="
prettyAssignOp XorA = "^="
prettyAssignOp OrA = "|="

message :: Op -> AssignOp -> String
message op assignOp =
  "Anstelle einer Anweisung x = x "
    ++ prettyOp op
    ++ " exp sollte x "
    ++ prettyAssignOp assignOp
    ++ " exp verwendet werden."
