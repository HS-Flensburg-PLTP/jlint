module Language.Java.Rules.MethodInvNumber where

import Config (MethodLimit (MethodLimit))
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: [MethodLimit] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check limits cUnit path = do
  MethodDecl span _ _ _ (Ident _ ident) _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  MethodLimit checkMethod limitMethod maxInv <- limits
  if ident == checkMethod
    then
      let limitedLength = length (filter (checkMethodInv limitMethod) (universeBi methodBody))
       in if limitedLength > maxInv
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.MethodInvNumber"
                    ("Die Methode " ++ checkMethod ++ " sollte die Methode " ++ limitMethod ++ " maximal " ++ show maxInv ++ "x aufrufen. Hier wird sie aber " ++ show limitedLength ++ "x aufgerufen.")
                    span
                    path
                )
            else mzero
    else mzero

checkMethodInv :: String -> MethodInvocation Parsed -> Bool
checkMethodInv limited (MethodCall _ _ (Ident _ ident) _) = ident == limited
checkMethodInv _ _ = False
