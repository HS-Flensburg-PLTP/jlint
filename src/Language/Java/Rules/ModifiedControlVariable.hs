module Language.Java.Rules.ModifiedControlVariable where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data
import qualified Data.List
import Language.Java.Pretty
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  basicFor <- extractBasicFor stmt
  checkBasicFor basicFor
  where
    extractBasicFor lol = case lol of
      BasicFor {} -> return lol
      _ -> mzero

    checkBasicFor (BasicFor (Just (ForLocalVars _ _ forVarDecls)) _ _ forStmt) = do
      varDeclIdent <- map (\(VarDecl (VarId varDeclIdentStr) _) -> varDeclIdentStr) forVarDecls
      expr <- universeBi forStmt
      checkBasicForModifiedControlVariable varDeclIdent expr
      where
        checkBasicForModifiedControlVariable varDeclIdent expr = case expr of
          Assign (NameLhs (Name assignIdents)) _ _ -> checkVarDeclIdentInExp varDeclIdent assignIdents
          PostIncrement (ExpName (Name name)) -> checkVarDeclIdentInExp varDeclIdent name
          PreIncrement (ExpName (Name name)) -> checkVarDeclIdentInExp varDeclIdent name
          PostDecrement (ExpName (Name name)) -> checkVarDeclIdentInExp varDeclIdent name
          PreDecrement (ExpName (Name name)) -> checkVarDeclIdentInExp varDeclIdent name
          _ -> mzero

        checkVarDeclIdentInExp :: Ident -> [Ident] -> [RDF.Diagnostic]
        checkVarDeclIdentInExp varDeclIdent expIdents =
          case Data.List.find (== varDeclIdent) expIdents of
            Nothing -> mzero
            Just _ -> return (RDF.rangeDiagnostic "ModifiedControlVariable" ("Laufvariable " ++ prettyPrint varDeclIdent ++ " darf nicht innerhalb der Schleife modifiziert werden!") dummySourceSpan path)
    checkBasicFor _ = mzero
