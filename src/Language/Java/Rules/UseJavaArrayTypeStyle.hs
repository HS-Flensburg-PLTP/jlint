module Language.Java.Rules.UseJavaArrayTypeStyle where

import Language.Java.Syntax
import qualified RDF
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = 
    checkFields cUnit path ++ checkLocalVars cUnit path

checkFields :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkFields cUnit path = do
    memberDecl <- universeBi cUnit
    checkMember memberDecl
    where
        checkMember (FieldDecl span _ _ varDecls) =
            concatMap (checkDeclaration path span) varDecls
        checkMember _ = mzero

checkLocalVars :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
checkLocalVars cUnit path = do
    block <- universeBi cUnit
    checkBlock block
    where
        checkBlock (LocalVars span _ _ varDecls) =
            concatMap (checkDeclaration path span) varDecls
        checkBlock _ = mzero

checkDeclaration :: FilePath -> SourceSpan -> VarDecl ->  [RDF.Diagnostic]
checkDeclaration path span (VarDecl (VarDeclArray _) _) =
    return 
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseJavaArrayTypeStyle"
            "Array Typen sollten in Java-Style und nicht C-Style definiert werden. Die Arrayklammern `[]` gehören also hinter den Typen und nicht hinter den Namen."
            span
            path
        )
        
checkDeclaration _ _ _ =
    mzero
