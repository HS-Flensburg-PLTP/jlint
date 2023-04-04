module Language.Java.Rules.InitializeVariables (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( AssignOp (EqualA),
    BlockStmt (BlockStmt, LocalVars),
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
      ( LocalVars _ _ _ varDecls
          : BlockStmt _ (ExpStmt range (Assign _ (NameLhs (Name [ident])) EqualA _))
          : _
        ) =
        let nonInitializedIdents = map VarDecl.ident (filter (not . VarDecl.isInitialized) varDecls)
         in if ident `elem` nonInitializedIdents
              then
                [ RDF.rangeDiagnostic
                    "Language.Java.Rules.InitializeVariables"
                    (message ident)
                    range
                    path
                ]
              else mzero
    checkBlocks _ = mzero

message :: Ident -> String
message ident =
  "Die Variable " ++ Markdown.code (Ident.name ident) ++ " wird nur deklariert, danach aber direkt initialisiert. Die Variable sollte bei der Deklaration daher schon initialisiert werden."
