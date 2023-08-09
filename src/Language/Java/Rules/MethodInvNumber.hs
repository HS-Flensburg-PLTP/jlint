module Language.Java.Rules.MethodInvNumber where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: String -> String -> Int -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check called limited maxInv cUnit path = do
  MethodDecl span _ _ _ (Ident _ ident) _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  if ident == called
    then
      let limitedLength = length (filter (checkMethodInv limited) (universeBi methodBody))
       in if limitedLength > maxInv
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.MethodInvNumber"
                    ("Die Methode " ++ called ++ " sollte die Methode " ++ limited ++ " maximal " ++ show maxInv ++ "x aufrufen. Hier wird sie aber " ++ show limitedLength ++ "x aufgerufen.")
                    span
                    path
                )
            else mzero
    else mzero

checkMethodInv :: String -> MethodInvocation Parsed -> Bool
checkMethodInv limited (MethodCall _ (Ident _ ident) _) = ident == limited
checkMethodInv _ _ = False
