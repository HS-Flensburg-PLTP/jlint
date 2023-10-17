module Language.Java.Rules.NoCasts (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Exp as Exp
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import QualifiedIdent (QualifiedIdent (..), hasClassName)
import qualified RDF

check :: [QualifiedIdent] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path =
  universeBi cUnit >>= checkClassDecl
  where
    checkClassDecl :: ClassDecl Parsed -> [RDF.Diagnostic]
    checkClassDecl (ClassDecl _ _ classIdent _ _ _ body) = do
      let methodNames = map methodName (filter (QualifiedIdent.hasClassName classIdent) whitelist)
      memberDecl <- universeBi body :: [MemberDecl Parsed]
      checkMethodDecl methodNames memberDecl
    checkClassDecl (EnumDecl _ _ enumIdent _ body) = do
      let methodNames = map methodName (filter (QualifiedIdent.hasClassName enumIdent) whitelist)
      memberDecl <- universeBi body :: [MemberDecl Parsed]
      checkMethodDecl methodNames memberDecl
    checkClassDecl _ = mzero

    checkMethodDecl :: [String] -> MemberDecl Parsed -> [RDF.Diagnostic]
    checkMethodDecl methodNames (MethodDecl span _ _ _ ident _ _ _ methodBody) = do
      if Ident.name ident `notElem` methodNames && any Exp.isCast (universeBi methodBody :: [Exp Parsed])
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.NoCasts"
                ["Die Methode", Markdown.code (Ident.name ident), "sollte keine Casts verwenden."]
                span
                path
            )
        else mzero
    checkMethodDecl _ _ = mzero
