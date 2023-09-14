module Language.Java.Rules.NoFurtherDataStructures where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.Syntax
import qualified Language.Java.Syntax.Exp.Extra as Exp.Extra
import qualified Language.Java.Syntax.Ident as Ident
import qualified RDF

check :: NonEmpty String -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check methodNames cUnit path = do
  MethodDecl span _ _ _ ident _ _ _ body <- universeBi cUnit :: [MemberDecl Parsed]
  methodName <- NonEmpty.toList methodNames
  if Ident.name ident == methodName && any Exp.Extra.isDataCreation (universeBi body :: [Exp Parsed])
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.NoFurtherDataStructures"
            ["In dieser Funktion sollten keine zusätzlichen Datenstrukturen erzeugt werden."]
            span
            path
        )
    else mzero
