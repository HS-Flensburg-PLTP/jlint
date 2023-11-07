module Language.Java.Rules.NoExtraDataStructures (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import qualified RDF

check :: NonEmpty String -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check methodNames cUnit path = do
  MethodDecl _ _ _ _ ident _ _ _ body <- universeBi cUnit :: [MemberDecl Parsed]
  if Ident.name ident `elem` methodNames
    then do
      span <-
        mapMaybe filterDataCreation (universeBi body :: [Exp Parsed])
          `mplus` mapMaybe filterCloneInvoke (universeBi body :: [MethodInvocation Parsed])
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.NoExtraDataStructures"
            [ "Bei der Implementierung der Methode "
                ++ Markdown.code (Ident.name ident)
                ++ " ist es nicht notwendig, eine zusätzliche Datenstruktur zu erzeugen.",
              "Um Speicherplatz und Laufzeit zu sparen, sollte man keine zusätzliche Datenstruktur erzeugen, wenn dies nicht notwendig ist."
            ]
            span
            path
        )
    else mzero

blacklist :: [String]
blacklist = ["ArrayList", "LinkedList", "HashMap", "HashSet", "TreeMap", "TreeSet", "SLList", "DLList"]

filterDataCreation :: Exp p -> Maybe SourceSpan
filterDataCreation (ArrayCreate span _ _ _) = Just span
filterDataCreation (InstanceCreation span _ typeDecl _ _)
  | isForbidden typeDecl = Just span
  | otherwise = Nothing
filterDataCreation (QualInstanceCreation span _ _ ident _ _)
  | Ident.name ident `elem` blacklist = Just span
  | otherwise = Nothing
filterDataCreation _ = Nothing

isForbidden :: TypeDeclSpecifier -> Bool
isForbidden (TypeDeclSpecifier (ClassType ((ident, []) :| []))) = Ident.name ident `elem` blacklist
isForbidden (TypeDeclSpecifier (ClassType _)) = False
isForbidden (TypeDeclSpecifierWithDiamond (ClassType ((ident, []) :| [])) _ _) = Ident.name ident `elem` blacklist
isForbidden (TypeDeclSpecifierWithDiamond (ClassType _) _ _) = False
isForbidden (TypeDeclSpecifierUnqualifiedWithDiamond ident _) = Ident.name ident `elem` blacklist

filterCloneInvoke :: MethodInvocation p -> Maybe SourceSpan
filterCloneInvoke (MethodCall span _ ident [])
  | Ident.name ident == "clone" = Just span
  | otherwise = Nothing
filterCloneInvoke _ = Nothing
