module Language.Java.Rules.RedundantModifiers where

import Control.Monad
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic, rangeDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  interfaceDecl <- universeBi cUnit
  case interfaceDecl of
    (InterfaceDecl _ InterfaceNormal _ _ _ _ _ body) -> do
      memberDecl <- universeBi body
      case memberDecl of
        (MethodDecl span modifiers _ _ _ _ _ _ _) ->
          concatMap (checkModifier path span) modifiers
        _ ->
          mzero
    _ ->
      mzero

checkModifier :: FilePath -> SourceSpan -> Modifier -> [Diagnostic]
checkModifier path span Public =
  return
    ( rangeDiagnostic
        "Language.Java.Rules.RedundantModifier"
        "Auf den redundanten Modifizierer `public` sollte in Interfaces verzichtet werden."
        span
        path
    )
checkModifier path span Abstract =
  return
    ( rangeDiagnostic
        "Language.Java.Rules.RedundantModifier"
        "Auf den redundanten Modifizierer `abstract` sollte in Interfaces verzichtet werden."
        span
        path
    )
checkModifier _ _ _ = mzero