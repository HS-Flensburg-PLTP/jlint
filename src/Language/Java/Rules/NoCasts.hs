module Language.Java.Rules.NoCasts where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import qualified Language.Java.HelperMethods.Exp as Exp
import Language.Java.Syntax
import qualified RDF

check :: [String] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  MethodDecl span _ _ _ (Ident _ methodName) _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  if methodName `notElem` whitelist && any Exp.isCast (universeBi methodBody :: [Exp Parsed])
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.NoCasts"
            ("Die Methode " ++ methodName ++ " sollte keine Casts beinhalten.")
            span
            path
        )
    else mzero
