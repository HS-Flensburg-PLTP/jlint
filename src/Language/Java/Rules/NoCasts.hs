module Language.Java.Rules.NoCasts where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: [String] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = do
  MethodDecl span _ _ _ (Ident _ methodName) _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  if methodName `notElem` whitelist && any checkMethodCast (universeBi methodBody)
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.NoCasts"
            ("Die Methode " ++ methodName ++ " sollte keine Casts beinhalten.")
            span
            path
        )
    else mzero

checkMethodCast :: Exp Parsed -> Bool
checkMethodCast (Cast _ _) = True
checkMethodCast _ = False