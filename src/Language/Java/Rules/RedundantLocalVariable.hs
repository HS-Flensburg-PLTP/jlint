module Language.Java.Rules.RedundantLocalVariable (check) where

import Control.Monad (mzero)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Exp.Extra as Exp.Extra
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

check :: Maybe Bool -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check Nothing _ _ = mzero
check (Just False) _ _ = mzero
check (Just True) cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks :: [BlockStmt Parsed] -> [RDF.Diagnostic]
    checkBlocks (LocalVars _ modifiers _ vars : stmt : stmts)
      | any isAnnotation modifiers = mzero
      | otherwise = concatMap checkVarDecl vars
      where
        checkVarDecl :: VarDecl Parsed -> [RDF.Diagnostic]
        checkVarDecl varDecl
          | shouldInline varDecl
              && not (isLoop stmt)
              && length (filter (eq IgnoreSourceSpan (VarDecl.ident varDecl)) (readVariables stmt)) == 1
              && not (any (eq IgnoreSourceSpan (VarDecl.ident varDecl)) (readVariables stmts)) =
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
          | otherwise = mzero
    checkBlocks _ = mzero

isLoop :: BlockStmt Parsed -> Bool
isLoop (BlockStmt stmt) = isLoop' stmt
  where
    isLoop' :: Stmt Parsed -> Bool
    isLoop' (While {}) = True
    isLoop' (Do {}) = True
    isLoop' (BasicFor {}) = True
    isLoop' (EnhancedFor {}) = True
    isLoop' _ = False
isLoop _ = False

readVariables :: Data a => a -> [Ident]
readVariables parent = universeBi parent >>= ident
  where
    ident :: Exp Parsed -> [Ident]
    ident (ExpName (Name _ (ident :| []))) = [ident]
    -- This is a workaround due to missing classification of locale variables
    ident (MethodInv (MethodCall _ (Just (Name _ (ident :| []))) _ _)) = [ident]
    ident _ = []

shouldInline :: VarDecl Parsed -> Bool
shouldInline (VarDecl _ _ Nothing) = True
shouldInline (VarDecl _ _ (Just varInit)) = shouldInlineVarInit varInit

shouldInlineVarInit :: VarInit Parsed -> Bool
shouldInlineVarInit (InitExp exp) = shouldInlineExp exp
shouldInlineVarInit (InitArray _) = False

-- While some expressions do not cause a side effect, we do not want to inline them anyway
shouldInlineExp :: Exp Parsed -> Bool
shouldInlineExp (Lit _) = True
shouldInlineExp (ClassLit _ _) = True
shouldInlineExp (This _) = True
shouldInlineExp (ThisClass {}) = True
shouldInlineExp (InstanceCreation {}) = False
shouldInlineExp (QualInstanceCreation {}) = False
shouldInlineExp (ArrayCreate {}) = False
shouldInlineExp (ArrayCreateInit {}) = True
shouldInlineExp (FieldAccess {}) = True
shouldInlineExp (MethodInv (MethodCall _ (Just (Name _ (Ident _ "System" :| []))) (Ident _ "nanoTime") [])) =
  False
shouldInlineExp (MethodInv (MethodCall _ _ (Ident _ "nextInt") [])) =
  False
shouldInlineExp (MethodInv _) = True
shouldInlineExp (ArrayAccess {}) = True
shouldInlineExp (ExpName {}) = True
shouldInlineExp (PostIncrement _ _) = False
shouldInlineExp (PostDecrement _ _) = False
shouldInlineExp (PreIncrement _ _) = False
shouldInlineExp (PreDecrement _ _) = False
shouldInlineExp (PrePlus _ exp) = Exp.Extra.hasNoSideEffect exp
shouldInlineExp (PreMinus _ exp) = Exp.Extra.hasNoSideEffect exp
shouldInlineExp (PreBitCompl _ exp) = Exp.Extra.hasNoSideEffect exp
shouldInlineExp (PreNot _ exp) = Exp.Extra.hasNoSideEffect exp
shouldInlineExp (SwitchExp {}) = False
shouldInlineExp (Cast {}) = True
shouldInlineExp (BinOp _ leftExp _ rightExp) = Exp.Extra.hasNoSideEffect leftExp && Exp.Extra.hasNoSideEffect rightExp
shouldInlineExp (InstanceOf _ exp _ _) = Exp.Extra.hasNoSideEffect exp
shouldInlineExp (Cond _ condExp thenExp elseExp) = all Exp.Extra.hasNoSideEffect [condExp, thenExp, elseExp]
shouldInlineExp (Assign {}) = False
shouldInlineExp (Lambda {}) = True
shouldInlineExp (MethodRef {}) = False

isAnnotation :: Modifier Parsed -> Bool
isAnnotation (Annotation _) = True
isAnnotation _ = False
