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
        checkBasicForModifiedControlVariable (Assign sourceSpan (NameLhs (Name name)) _ _) = checkVarDeclIdentInExp name sourceSpan
        checkBasicForModifiedControlVariable (PostIncrement sourceSpan (ExpName (Name name))) = checkVarDeclIdentInExp name sourceSpan
        checkBasicForModifiedControlVariable (PreIncrement sourceSpan (ExpName (Name name))) = checkVarDeclIdentInExp name sourceSpan
        checkBasicForModifiedControlVariable (PostDecrement sourceSpan (ExpName (Name name))) = checkVarDeclIdentInExp name sourceSpan
        checkBasicForModifiedControlVariable (PreDecrement sourceSpan (ExpName (Name name))) = checkVarDeclIdentInExp name sourceSpan
        checkBasicForModifiedControlVariable _ = mzero

        checkVarDeclIdentInExp :: [Ident] -> SourceSpan -> [RDF.Diagnostic]
        checkVarDeclIdentInExp expIdents sourceSpan =
          [ RDF.rangeDiagnostic
              "Language.Java.Rules.ModifiedControlVariable"
              ("Laufvariable " ++ prettyPrint (ident varDecl) ++ " darf nicht innerhalb der Schleife modifiziert werden!")
              sourceSpan
              path
            | varDecl <- filter (\varDecl -> ident varDecl `elem` expIdents) forVarDecls
          ]
    checkBasicFor _ = mzero
