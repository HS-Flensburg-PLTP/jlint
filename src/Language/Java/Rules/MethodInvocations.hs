module Language.Java.Rules.MethodInvocations (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: String -> String -> Int -> String -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check targetMethod limitedMethod maxInv explanation cUnit path = do
  MethodDecl span _ _ _ (Ident _ name) _ _ _ methodBody <- universeBi cUnit :: [MemberDecl Parsed]
  if name == targetMethod
    then
      let noInv = length (filter (isInvocationOf limitedMethod) (universeBi methodBody))
       in if noInv > maxInv
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.MethodInvocations"
                    ( [ "Die Methode",
                        Markdown.code targetMethod,
                        "sollte die Methode",
                        Markdown.code limitedMethod
                      ]
                        ++ ( if maxInv == 0
                               then ["nicht verwenden."]
                               else
                                 [ "maximal",
                                   show maxInv ++ "-mal",
                                   "aufrufen. Hier wird sie aber",
                                   show noInv ++ "-mal",
                                   "aufgerufen."
                                 ]
                           )
                        ++ [explanation]
                    )
                    span
                    path
                )
            else mzero
    else mzero

isInvocationOf :: String -> MethodInvocation Parsed -> Bool
isInvocationOf limited (MethodCall _ _ (Ident _ name) _) = name == limited
isInvocationOf _ _ = False
