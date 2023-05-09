module Language.Java.Rules.PreferExpressions (check) where

import Control.Monad (MonadPlus (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Extra (none)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks
      ( LocalVars (startLoc, _) _ _ vars
          : BlockStmt _ (ExpStmt (_, endLoc) exp)
          : _
        ) =
        case filterVarUpdate exp of
          Nothing -> mzero
          Just var ->
            let declVars = map VarDecl.ident (filter VarDecl.isInitialized vars)
             in if any (eq IgnoreSourceSpan var) declVars
                  then
                    return
                      ( RDF.rangeDiagnostic
                          "Language.Java.Rules.PreferExpressions"
                          (assignedTwiceMessage var)
                          (startLoc, endLoc)
                          path
                      )
                  else mzero
    checkBlocks
      ( BlockStmt _ (ExpStmt (startLoc, _) exp1)
          : BlockStmt _ (ExpStmt (_, endLoc) exp2)
          : _
        ) =
        case (filterVarUpdate exp1, filterVarUpdate exp2) of
          (Nothing, _) -> mzero
          (Just _, Nothing) -> mzero
          (Just ident1, Just ident2) ->
            if eq IgnoreSourceSpan ident1 ident2
              then
                return
                  ( RDF.rangeDiagnostic
                      "Language.Java.Rules.PreferExpressions"
                      (assignedTwiceMessage ident1)
                      (startLoc, endLoc)
                      path
                  )
              else mzero
    checkBlocks
      ( BlockStmt _ (ExpStmt span exp)
          : stmt
          : stmts
        ) =
        case filterVarUpdate exp of
          Nothing -> mzero
          Just ident ->
            if length (filter (eq IgnoreSourceSpan ident) (variablesRead stmt)) == 1
              && none (eq IgnoreSourceSpan ident) (variablesWritten stmt)
              && none (eq IgnoreSourceSpan ident) (variablesRead stmts)
              then
                return
                  ( RDF.rangeDiagnostic
                      "Language.Java.Rules.PreferExpressions"
                      (assignedAndUsedMessage ident)
                      span
                      path
                  )
              else mzero
    checkBlocks _ = mzero

filterVarUpdate :: Exp -> Maybe Ident
filterVarUpdate (PostIncrement _ (ExpName (Name _ [ident]))) = Just ident
filterVarUpdate (PostDecrement _ (ExpName (Name _ [ident]))) = Just ident
filterVarUpdate (PreIncrement _ (ExpName (Name _ [ident]))) = Just ident
filterVarUpdate (PreDecrement _ (ExpName (Name _ [ident]))) = Just ident
filterVarUpdate (Assign _ (NameLhs (Name _ [ident])) _ _) = Just ident
filterVarUpdate _ = Nothing

variablesRead :: (Data a) => a -> [Ident]
variablesRead parent = [ident | ExpName (Name _ idents) <- universeBi parent, ident <- idents]

variablesWritten :: (Data a) => a -> [Ident]
variablesWritten parent = [ident | Assign _ (NameLhs (Name _ [ident])) _ _ <- universeBi parent]

assignedTwiceMessage :: Ident -> String
assignedTwiceMessage ident =
  "Der Variable " ++ Markdown.code (Ident.name ident) ++ " wird zweimal direkt nacheinander ein neuer Wert zugewiesen. Diese beiden Zuweisungen können zusammengefasst werden."

assignedAndUsedMessage :: Ident -> String
assignedAndUsedMessage ident =
  "Der Variable " ++ Markdown.code (Ident.name ident) ++ " wird ein neuer Wert zugewiesen und sie wird direkt danach nur gelesen.\nDaher kann der Wert, der der Variable zugewiesen wird, dort eingesetzt werden, wo die Variable verwendet wird."
