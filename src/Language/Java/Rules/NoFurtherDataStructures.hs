module Language.Java.Rules.NoFurtherDataStructures (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified RDF

check :: NonEmpty String -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check methodNames cUnit path = do
  MethodDecl span _ _ _ ident _ _ _ body <- universeBi cUnit :: [MemberDecl Parsed]
  if Ident.name ident `elem` methodNames && any isDataCreation (universeBi body :: [Exp Parsed])
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.NoFurtherDataStructures"
            ["In dieser Funktion sollten keine zusätzlichen Datenstrukturen erzeugt werden."]
            span
            path
        )
    else mzero

isDataCreation :: Exp p -> Bool
isDataCreation (ArrayCreate {}) = True
isDataCreation (InstanceCreation {}) = True
isDataCreation (QualInstanceCreation {}) = True
isDataCreation _ = False
