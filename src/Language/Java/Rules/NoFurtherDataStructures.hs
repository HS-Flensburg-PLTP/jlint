module Language.Java.Rules.NoFurtherDataStructures where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import Language.Java.Syntax.BlockStmt (extractVarDecls)
import qualified Language.Java.Syntax.Ident as Ident
import qualified RDF

check :: [String] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check methodNames cUnit path = do
  MethodDecl span _ _ _ ident _ _ _ body <- universeBi cUnit
  InstanceCreation {} <- universeBi cUnit
  methodName <- methodNames
  checkMethod methodName ident body span path

checkMethod :: String -> Ident -> MethodBody Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkMethod methodName ident body span path =
  if Ident.name ident == methodName
    then checkMethodBody body (Ident.name ident) span path
    else mzero

checkMethodBody :: MethodBody Parsed -> String -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkMethodBody (MethodBody (Just (Block blockstmts))) methodName span path = do
  blockstmt <- blockstmts
  VarDecl _ _ (Just varInit) <- extractVarDecls blockstmt
  checkVarInits varInit span path methodName
checkMethodBody _ _ _ _ = mzero

checkVarInits :: VarInit Parsed -> SourceSpan -> FilePath -> String -> [RDF.Diagnostic]
checkVarInits (InitExp (InstanceCreation {})) span path methodName =
  return
    ( RDF.rangeDiagnostic
        "Language.Java.Rules.NoFurtherDataStructures"
        ("In der Funktion " ++ methodName ++ " sollten keine Datenstrukturen erzeugt werden.")
        span
        path
    )
checkVarInits (InitExp (ArrayCreate {})) span path methodName =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.NoFurtherDataStructures"
      ("In der Funktion " ++ methodName ++ " sollte kein Array erzeugt werden.")
      span
      path
  ]
checkVarInits _ _ _ _ = mzero
