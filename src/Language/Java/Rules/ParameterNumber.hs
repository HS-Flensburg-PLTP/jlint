module Language.Java.Rules.ParameterNumber (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (fromMaybe)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import qualified RDF

check :: Maybe Int -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check max cUnit path =
  let maxVal = fromMaybe 7 max
   in do
        MethodDecl span _ _ _ ident paramList _ _ _ <- universeBi cUnit :: [MemberDecl Parsed]
        if length paramList > maxVal
          then
            return
              ( RDF.rangeDiagnostic
                  "Language.Java.Rules.ParameterNumber"
                  [ "Die Methode",
                    Markdown.code (Ident.name ident),
                    "hat",
                    show (length paramList),
                    "Parameter. Es sollten aber nicht mehr als",
                    show maxVal,
                    "Parameter für eine Methode verwendet werden."
                  ]
                  span
                  path
              )
          else mzero
