module Language.Java.Rules.NoCasts (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Exp as Exp
import qualified Language.Java.Syntax.Ident as Ident
import qualified RDF

check :: [String] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  MethodDecl span _ _ _ ident _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  if Ident.name ident `notElem` whitelist && any Exp.isCast (universeBi methodBody :: [Exp Parsed])
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.NoCasts"
            ["Die Methode", Ident.name ident, "sollte keine Casts verwenden."]
            span
            path
        )
    else mzero
