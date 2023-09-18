module Language.Java.Rules.MethodInvNumber (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: String -> String -> Int -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check called limited maxInv cUnit path = do
  MethodDecl span _ _ _ (Ident _ name) _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  if name == called
    then
      let noInv = length (filter (isInvocationOf limited) (universeBi methodBody))
       in if noInv > maxInv
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.MethodInvNumber"
                    [ "Die Methode",
                      called,
                      "sollte die Methode",
                      limited,
                      "maximal",
                      show maxInv ++ "-mal",
                      "aufrufen. Hier wird sie aber",
                      show noInv ++ "-mal",
                      "aufgerufen."
                    ]
                    span
                    path
                )
            else mzero
    else mzero

isInvocationOf :: String -> MethodInvocation Parsed -> Bool
isInvocationOf limited (MethodCall _ _ (Ident _ name) _) = name == limited
isInvocationOf _ _ = False
