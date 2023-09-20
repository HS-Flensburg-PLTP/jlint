module Language.Java.Rules.DeclarationOrder (check) where

import Control.Monad (mzero)
import Data.Foldable (toList)
import Data.Function (on, (&))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF
import qualified String

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkRank path (extractMemberDecls cUnit)

extractMemberDecls :: CompilationUnit Parsed -> [RankedIdents]
extractMemberDecls cUnit = mapMaybe checkTopLevelDeclaration (universeBi cUnit)

checkTopLevelDeclaration :: MemberDecl Parsed -> Maybe RankedIdents
checkTopLevelDeclaration (FieldDecl sourceSpan mods _ varDecls)
  | any Modifier.isStatic mods =
      if any Modifier.isPublic mods
        then return (RankedIdents idents StaticPublicField sourceSpan)
        else
          if any Modifier.isProtected mods
            then return (RankedIdents idents StaticProtectedField sourceSpan)
            else
              if any Modifier.isPrivate mods
                then return (RankedIdents idents StaticPrivateField sourceSpan)
                else return (RankedIdents idents StaticPackageField sourceSpan)
  | any Modifier.isPublic mods = return (RankedIdents idents InstancePublicField sourceSpan)
  | any Modifier.isProtected mods = return (RankedIdents idents InstanceProtectedField sourceSpan)
  | any Modifier.isPrivate mods = return (RankedIdents idents InstancePrivateField sourceSpan)
  | otherwise = return (RankedIdents idents InstancePackageField sourceSpan)
  where
    idents = fmap VarDecl.ident varDecls
checkTopLevelDeclaration (ConstructorDecl sourceSpan _ _ ident _ _ _) =
  return (RankedIdents (return ident) Constructor sourceSpan)
checkTopLevelDeclaration (MethodDecl sourceSpan _ _ _ ident _ _ _ _) =
  return (RankedIdents (return ident) Method sourceSpan)
checkTopLevelDeclaration _ = mzero

data Rank
  = StaticPublicField
  | StaticProtectedField
  | StaticPackageField
  | StaticPrivateField
  | InstancePublicField
  | InstanceProtectedField
  | InstancePackageField
  | InstancePrivateField
  | Constructor
  | Method
  deriving (Eq, Ord)

rankToString :: Int -> Rank -> String
rankToString n StaticPublicField =
  String.plural n "Die öffentliche, statische Variable" "Die öffentlichen, statischen Variablen"
rankToString n StaticProtectedField =
  String.plural n "Die geschützte, statische Variable" "Die geschützten, statischen Variablen"
rankToString n StaticPackageField =
  String.plural n "Die statische Paket-Variable" "Die statischen Paket-Variablen"
rankToString n StaticPrivateField =
  String.plural n "Die private, statische Variable" "Die privaten, statischen Variablen"
rankToString n InstancePublicField =
  String.plural n "Das öffentliche Attribut" "Die öffentlichen Attribute"
rankToString n InstanceProtectedField =
  String.plural n "Das geschützte Attribut" "Die geschützten Attribute"
rankToString n InstancePackageField =
  String.plural n "Das Paket-Attribut" "Die Paket-Attribute"
rankToString n InstancePrivateField =
  String.plural n "Das private Attribut" "Die privaten Attribute"
rankToString n Constructor =
  String.plural n "Der Konstruktor" "Die Konstruktoren"
rankToString n Method =
  String.plural n "Die Methode" "Die Methoden"

checkRank :: FilePath -> [RankedIdents] -> [RDF.Diagnostic]
checkRank path declOrder =
  zip declOrder (tail declOrder)
    & filter (uncurry ((>) `on` rank))
    & map (createError . snd)
  where
    createError :: RankedIdents -> RDF.Diagnostic
    createError (RankedIdents idents rank span) =
      RDF.rangeDiagnostic
        "Language.Java.Rules.DeclarationOrder"
        [ rankToString (length idents) rank,
          String.enumerate (toList (fmap (Markdown.code . Ident.name) idents)),
          String.plural (length idents) "ist" "sind",
          "an der falschen Stelle deklariert.",
          "Die Java-Konventionen geben eine Reihenfolge vor, in der die verschiedenen Deklarationen auftreten sollten."
        ]
        span
        path

-- Helping data structures

data RankedIdents = RankedIdents (NonEmpty Ident) Rank SourceSpan

rank :: RankedIdents -> Rank
rank (RankedIdents _ r _) = r
