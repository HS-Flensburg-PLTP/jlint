module Language.Java.Rules.ModifiedControlVariable where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty
import Language.Java.Syntax
import Language.Java.Syntax.VarDecl (ident)
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkBasicFor stmt
  where
    checkBasicFor (BasicFor (Just (ForLocalVars _ _ forVarDecls)) _ _ forStmt) = do
      expr <- universeBi forStmt
      checkBasicForModifiedControlVariable expr
      where
        checkBasicForModifiedControlVariable (Assign (NameLhs (Name name)) _ _) = checkVarDeclIdentInExp name
        checkBasicForModifiedControlVariable (PostIncrement (ExpName (Name name))) = checkVarDeclIdentInExp name
        checkBasicForModifiedControlVariable (PreIncrement (ExpName (Name name))) = checkVarDeclIdentInExp name
        checkBasicForModifiedControlVariable (PostDecrement (ExpName (Name name))) = checkVarDeclIdentInExp name
        checkBasicForModifiedControlVariable (PreDecrement (ExpName (Name name))) = checkVarDeclIdentInExp name
        checkBasicForModifiedControlVariable _ = mzero

        checkVarDeclIdentInExp :: [Ident] -> [RDF.Diagnostic]
        checkVarDeclIdentInExp expIdents =
          [ RDF.rangeDiagnostic
              "Language.Java.Rules.ModifiedControlVariable"
              ("Laufvariable " ++ prettyPrint (ident varDecl) ++ " darf nicht innerhalb der Schleife modifiziert werden!")
              dummySourceSpan
              path
            | varDecl <- filter (\varDecl -> ident varDecl `elem` expIdents) forVarDecls
          ]
    checkBasicFor _ = mzero
