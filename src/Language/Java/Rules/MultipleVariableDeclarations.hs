module Language.Java.Rules.MultipleVariableDeclarations (check) where

import Control.Monad (mplus, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  mapMaybe checkMemberDecl (universeBi cUnit) `mplus` mapMaybe checkBlockStmt (universeBi cUnit)
  where
    checkMemberDecl :: MemberDecl Parsed -> Maybe RDF.Diagnostic
    checkMemberDecl (FieldDecl span _ _ fieldDecls) =
      if length fieldDecls > 1
        then return (message FieldVar span path)
        else mzero
    checkMemberDecl _ = mzero
    checkBlockStmt :: BlockStmt Parsed -> Maybe RDF.Diagnostic
    checkBlockStmt (LocalVars span _ _ varDecls) =
      if length varDecls > 1
        then return (message LocalVar span path)
        else mzero
    checkBlockStmt _ = mzero

data VarType = LocalVar | FieldVar

varTypeToString :: VarType -> String
varTypeToString LocalVar = "Variablen"
varTypeToString FieldVar = "Attribute"

message :: VarType -> SourceSpan -> FilePath -> RDF.Diagnostic
message varType =
  RDF.rangeDiagnostic
    "Language.Java.Rules.MultipleVariableDeclarations"
    ["Das Deklarieren mehrerer", varTypeToString varType, "in der gleichen Zeile sollte vermieden werden."]
