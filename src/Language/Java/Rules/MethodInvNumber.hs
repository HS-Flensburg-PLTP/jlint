module Language.Java.Rules.MethodInvNumber where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: String -> String -> Int -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check called limited maxInv cUnit path = do
  memberDecl <- universeBi cUnit :: [MemberDecl Parsed]
  checkMemberDecl memberDecl
  where
    checkMemberDecl (MethodDecl span _ _ _ (Ident _ ident) _ _ _ methodBody)
      | ident == called =
          if length (filter (checkMethodInv limited) (universeBi methodBody)) > maxInv
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.MethodInvNumber"
                    ("Die Methode " ++ called ++ " sollte die Methode " ++ limited ++ " maximal " ++ show maxInv ++ "x aufrufen.")
                    span
                    path
                )
            else mzero
      | otherwise = mzero
    checkMemberDecl _ = mzero

checkMethodInv :: String -> MethodInvocation Parsed -> Bool
checkMethodInv limited (MethodCall _ (Ident _ ident) _) = ident == limited
checkMethodInv _ _ = False
