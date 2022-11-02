{-# LANGUAGE FlexibleContexts #-}

module Language.Java.Rules.ReduceScope where

import Control.Monad (MonadPlus (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
  ( Block (Block),
    BlockStmt (BlockStmt, LocalVars),
    CompilationUnit,
    Exp (..),
    Ident (Ident),
    Name (Name),
    Stmt (..),
    VarDecl (..),
    VarDeclId (..),
  )
import qualified Markdown
import RDF (Diagnostic, rangeDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  blocks <- universeBi cUnit
  checkBlocks blocks
  where
    checkBlocks (LocalVars range _ _ vars : BlockStmt stmt : stmts) =
      let declVars = map varDeclName vars
          varsNotInStmt = filter (`notElem` variables stmt) declVars
          varsNotInStmts = filter (`notElem` variables stmts) declVars
       in if hasNoSideEffect stmt
            then
              if null varsNotInStmt
                then reduceVarsInIf varsNotInStmts stmt path
                else
                  map
                    ( \var ->
                        rangeDiagnostic
                          "Language.Java.Rules.ReduceScope"
                          (message var)
                          range
                          path
                    )
                    varsNotInStmt
            else reduceVarsInIf varsNotInStmts stmt path
    checkBlocks _ = mzero

reduceVarsInIf :: [Ident] -> Stmt -> FilePath -> [Diagnostic]
reduceVarsInIf declVars (IfThenElse range condition thenStmt elseStmt) path =
  let varsNotInCondition = filter (`notElem` variables condition) declVars
   in if hasNoSideEffectExp condition
        then
          if null varsNotInCondition
            then mzero
            else
              let varsNotInThenOrElse =
                    filter
                      (\var -> var `notElem` variables thenStmt || var `notElem` variables elseStmt)
                      varsNotInCondition
               in if not (null varsNotInThenOrElse)
                    then
                      map
                        ( \var ->
                            rangeDiagnostic
                              "Language.Java.Rules.ReduceScope"
                              (message var)
                              range
                              path
                        )
                        varsNotInThenOrElse
                    else mzero
        else mzero
reduceVarsInIf _ _ _ = mzero

message :: Ident -> String
message var =
  "Der Scope der Variable " ++ Markdown.code (nameFromIdent var) ++ " kann reduziert werden."

varDeclName :: VarDecl -> Ident
varDeclName (VarDecl varDeclId _) = varDeclIdName varDeclId

varDeclIdName :: VarDeclId -> Ident
varDeclIdName (VarDeclArray varDeclId) = varDeclIdName varDeclId
varDeclIdName (VarId ident) = ident

variables :: (Data a) => a -> [Ident]
variables parent = [ident | ExpName (Name idents) <- universeBi parent, ident <- idents]

nameFromIdent :: Ident -> String
nameFromIdent (Ident name) = name

-- Identifies statements that do not cause side effects concerning class attributes
hasNoSideEffect :: Stmt -> Bool
hasNoSideEffect (StmtBlock (Block _)) = False
hasNoSideEffect (IfThen {}) = False
hasNoSideEffect (IfThenElse {}) = False
hasNoSideEffect (While {}) = False
hasNoSideEffect (BasicFor {}) = False
hasNoSideEffect (EnhancedFor {}) = False
hasNoSideEffect Empty = False
hasNoSideEffect (ExpStmt _) = False
hasNoSideEffect (Assert {}) = False
hasNoSideEffect (Switch {}) = False
hasNoSideEffect (Do {}) = False
hasNoSideEffect (Break {}) = True
hasNoSideEffect (Continue {}) = True
hasNoSideEffect (Return {}) = True
hasNoSideEffect (Synchronized {}) = False
hasNoSideEffect (Throw {}) = False
hasNoSideEffect (Try {}) = False
hasNoSideEffect (Labeled _ stmt) = hasNoSideEffect stmt

hasNoSideEffectExp :: Exp -> Bool
hasNoSideEffectExp (Lit _) = True
hasNoSideEffectExp (ClassLit _) = False
hasNoSideEffectExp This = False
hasNoSideEffectExp (ThisClass {}) = False
hasNoSideEffectExp (InstanceCreation {}) = False
hasNoSideEffectExp (QualInstanceCreation {}) = False
hasNoSideEffectExp (ArrayCreate {}) = False
hasNoSideEffectExp (ArrayCreateInit {}) = False
hasNoSideEffectExp (FieldAccess {}) = True
hasNoSideEffectExp (MethodInv {}) = False
hasNoSideEffectExp (ArrayAccess {}) = False
hasNoSideEffectExp (ExpName {}) = True
hasNoSideEffectExp (PostIncrement _) = False
hasNoSideEffectExp (PostDecrement _) = False
hasNoSideEffectExp (PreIncrement _) = False
hasNoSideEffectExp (PreDecrement _) = False
hasNoSideEffectExp (PrePlus {}) = False
hasNoSideEffectExp (PreMinus {}) = False
hasNoSideEffectExp (PreBitCompl {}) = False
hasNoSideEffectExp (PreNot {}) = False
hasNoSideEffectExp (SwitchExp {}) = False
hasNoSideEffectExp (Cast {}) = False
hasNoSideEffectExp (BinOp leftExp _ rightExp) = hasNoSideEffectExp leftExp && hasNoSideEffectExp rightExp
hasNoSideEffectExp (InstanceOf {}) = False
hasNoSideEffectExp (Cond {}) = False
hasNoSideEffectExp (Assign {}) = False
hasNoSideEffectExp (Lambda {}) = False
hasNoSideEffectExp (MethodRef {}) = False
