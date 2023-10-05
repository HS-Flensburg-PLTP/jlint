module Language.Java.Rules.RedundantLocalVariable (check) where

import Control.Monad (mzero)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Language.Java.Syntax.VarDecl.Extra as VarDecl.Extra
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks :: [BlockStmt Parsed] -> [RDF.Diagnostic]
    checkBlocks (LocalVars _ _ _ vars : BlockStmt stmt : stmts) =
      concatMap checkVarDecl vars
      where
        checkVarDecl varDecl =
          if isSimpleInit varDecl
            && VarDecl.Extra.hasNoSideEffect varDecl
            && not (any (eq IgnoreSourceSpan (VarDecl.ident varDecl)) (variables stmts))
            && length (filter (eq IgnoreSourceSpan (VarDecl.ident varDecl)) (variables stmt)) == 1
            && not (isLoop stmt)
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.RedundantLocalVariable"
                    [ "Die Variable",
                      Markdown.code (prettyPrint (VarDecl.ident varDecl)),
                      "wird deklariert und direkt danach nur einmal verwendet.",
                      "In diesem Fall sollte auf die Deklaration der Variable verzichtet werden."
                    ]
                    (sourceSpan varDecl)
                    path
                )
            else mzero
    checkBlocks _ = mzero

variables :: (Data a) => a -> [Ident]
variables parent =
  [ident | Name _ (ident :| []) <- universeBi parent]

isLoop :: Stmt p -> Bool
isLoop (While {}) = True
isLoop (Do {}) = True
isLoop (BasicFor {}) = True
isLoop (EnhancedFor {}) = True
isLoop _ = False

isSimpleInit :: VarDecl p -> Bool
isSimpleInit (VarDecl _ _ Nothing) = True
isSimpleInit (VarDecl _ _ (Just (InitArray _))) = False
isSimpleInit (VarDecl _ _ (Just _)) = True
