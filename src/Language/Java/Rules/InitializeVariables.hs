module Language.Java.Rules.InitializeVariables (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkBlocks (universeBi cUnit)
  where
    checkBlocks :: [BlockStmt Parsed] -> Maybe RDF.Diagnostic
    checkBlocks
      ( LocalVars _ _ _ varDecls
          : BlockStmt (ExpStmt range (Assign _ (NameLhs (Name _ (ident :| []))) EqualA _))
          : _
        ) =
        let nonInitializedIdents = map VarDecl.ident (NonEmpty.filter (not . VarDecl.isInitialized) varDecls)
         in if any (eq IgnoreSourceSpan ident) nonInitializedIdents
              then
                return
                  ( RDF.rangeDiagnostic
                      "Language.Java.Rules.InitializeVariables"
                      (message ident)
                      range
                      path
                  )
              else mzero
    checkBlocks _ = mzero

message :: Ident -> [String]
message ident =
  [ "Die Variable",
    Markdown.code (Ident.name ident),
    "wird nur deklariert, danach aber direkt initialisiert.",
    "Die Variable sollte bei der Deklaration daher schon initialisiert werden."
  ]
