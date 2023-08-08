{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.UseAssignOp (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt :: Stmt Parsed <- universeBi cUnit
  checkStmt stmt
  where
    checkStmt (ExpStmt range (Assign _ (NameLhs name1) EqualA (BinOp _ (ExpName name2) op _))) =
      if eq IgnoreSourceSpan name1 name2
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

message :: Op -> AssignOp -> String
message op assignOp =
  "Anstelle einer Anweisung x = x "
    ++ prettyPrint op
    ++ " exp sollte x "
    ++ prettyPrint assignOp
    ++ " exp verwendet werden."
