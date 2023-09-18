module Language.Java.Rules.DeclarationOrder (check) where

import Control.Monad (mzero)
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkRank path (extractMemberDecls cUnit)

extractMemberDecls :: CompilationUnit Parsed -> [(Rank, SourceSpan)]
extractMemberDecls cUnit = mapMaybe checkTopLevelDeclaration (universeBi cUnit)

checkTopLevelDeclaration :: MemberDecl Parsed -> Maybe (Rank, SourceSpan)
checkTopLevelDeclaration (FieldDecl sourceSpan mods _ _)
  | any Modifier.isStatic mods =
      if any Modifier.isPublic mods
        then return (StaticPublicField, sourceSpan)
        else
          if any Modifier.isProtected mods
            then return (StaticProtectedField, sourceSpan)
            else
              if any Modifier.isPrivate mods
                then return (StaticPrivateField, sourceSpan)
                else return (StaticPackageField, sourceSpan)
  | any Modifier.isPublic mods = return (InstancePublicField, sourceSpan)
  | any Modifier.isProtected mods = return (InstanceProtectedField, sourceSpan)
  | any Modifier.isPrivate mods = return (InstancePrivateField, sourceSpan)
  | otherwise = return (InstancePackageField, sourceSpan)
checkTopLevelDeclaration (ConstructorDecl sourceSpan _ _ _ _ _ _) = return (Constructor, sourceSpan)
checkTopLevelDeclaration (MethodDecl sourceSpan _ _ _ _ _ _ _ _) = return (Method, sourceSpan)
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

rankToString :: Rank -> String
rankToString StaticPublicField = "Die öffentliche, statische Variable"
rankToString StaticProtectedField = "Die geschützte, statische Variable"
rankToString StaticPackageField = "Die statische Paket-Variable"
rankToString StaticPrivateField = "Die private, statische Variable"
rankToString InstancePublicField = "Das öffentliche Attribut"
rankToString InstanceProtectedField = "Das geschützte Attribut"
rankToString InstancePackageField = "Das Paket-Attribut"
rankToString InstancePrivateField = "Das private Attribut"
rankToString Constructor = "Der Konstruktor"
rankToString Method = "Die Methode"

checkRank :: FilePath -> [(Rank, SourceSpan)] -> [RDF.Diagnostic]
checkRank path declOrder =
  zip declOrder (tail declOrder)
    & filter (\((rankA, _), (rankB, _)) -> rankA > rankB)
    & map (createError . snd)
  where
    createError :: (Rank, SourceSpan) -> RDF.Diagnostic
    createError (rank, sourcespan) =
      RDF.rangeDiagnostic
        "Language.Java.Rules.DeclarationOrder"
        [ rankToString rank,
          "ist an der falschen Stelle deklariert. Die Java-Konventionen geben eine Reihenfolge vor, in der die verschiedenen Deklarationen auftreten sollten."
        ]
        sourcespan
        path
