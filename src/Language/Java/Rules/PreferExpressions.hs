module Language.Java.Rules.PreferExpressions (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( BlockStmt (BlockStmt, LocalVars),
    CompilationUnit,
    Exp (..),
    Ident,
    Lhs (NameLhs),
    Name (Name),
    Stmt (ExpStmt),
  )
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
          : BlockStmt (ExpStmt (_, endLoc) exp)
          : _
        ) =
        case filterVarUpdate exp of
          Nothing -> mzero
          Just var ->
            let declVars = map VarDecl.ident (filter VarDecl.isInitialized vars)
             in if var `elem` declVars
                  then
                    [ RDF.rangeDiagnostic
                        "Language.Java.Rules.PreferExpressions"
                        (message var)
                        (startLoc, endLoc)
                        path
                    ]
                  else mzero
    checkBlocks
      ( BlockStmt (ExpStmt (startLoc, _) exp1)
          : BlockStmt (ExpStmt (_, endLoc) exp2)
          : _
        ) =
        case (filterVarUpdate exp1, filterVarUpdate exp2) of
          (Nothing, _) -> mzero
          (_, Nothing) -> mzero
          (Just ident1, Just ident2) ->
            if ident1 == ident2
              then
                [ RDF.rangeDiagnostic
                    "Language.Java.Rules.PreferExpressions"
                    (message ident1)
                    (startLoc, endLoc)
                    path
                ]
              else mzero
    checkBlocks _ = mzero

filterVarUpdate :: Exp -> Maybe Ident
filterVarUpdate (PostIncrement (ExpName (Name [ident]))) = Just ident
filterVarUpdate (PostDecrement (ExpName (Name [ident]))) = Just ident
filterVarUpdate (PreIncrement (ExpName (Name [ident]))) = Just ident
filterVarUpdate (PreDecrement (ExpName (Name [ident]))) = Just ident
filterVarUpdate (Assign (NameLhs (Name [ident])) _ _) = Just ident
filterVarUpdate _ = Nothing

message :: Ident -> String
message ident =
  "Der Variable " ++ Markdown.code (Ident.name ident) ++ " wird zweimal direkt nacheinander ein neuer Wert zugewiesen. Diese beiden Zuweisungen können zusammengefasst werden."
