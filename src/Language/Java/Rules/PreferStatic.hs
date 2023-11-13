module Language.Java.Rules.PreferStatic (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import Language.Java.Transformer (transformCompilationUnitToAnalyzed)
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  universeBi (transformCompilationUnitToAnalyzed cUnit) >>= checkMemberDecl
  where
    checkMemberDecl :: MemberDecl Analyzed -> [RDF.Diagnostic]
    checkMemberDecl (MethodDecl span modifiers _ _ ident _ _ _ methodBody)
      | not (any Modifier.isStatic modifiers)
          && null (mapMaybe filterAttribute (universeBi methodBody)) =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.PreferStatic"
                [ "Die Methode",
                  Markdown.code (Ident.name ident),
                  "verwendet kein Attribut und sollte daher statisch definiert werden."
                ]
                span
                path
            )
      | otherwise = mzero
    checkMemberDecl _ = mzero

    filterAttribute :: FieldAccess Analyzed -> Maybe Ident
    filterAttribute (PrimaryFieldAccess _ _ ident) = Just ident
    filterAttribute (SuperFieldAccess _ ident) = Just ident
    filterAttribute (ClassFieldAccess _ _ ident) = Just ident
