module Language.Java.Rules.DeclarationOrder (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (maybeToList)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Modifier as Modifier
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkRank path (extractMemberDecls cUnit)

extractMemberDecls :: CompilationUnit Parsed -> [(Rank, SourceSpan)]
extractMemberDecls cUnit = do
  toplvlDecl <- universeBi cUnit
  maybeToList (checkTopLevelStmts toplvlDecl)

checkTopLevelStmts :: MemberDecl Parsed -> Maybe (Rank, SourceSpan)
checkTopLevelStmts toplvlDecl = case toplvlDecl of
  FieldDecl sourceSpan mods _ _ ->
    if any Modifier.isStatic mods
      then
        if any Modifier.isPublic mods
          then Just (StaticPublicField, sourceSpan)
          else
            if any Modifier.isProtected mods
              then Just (StaticProtectedField, sourceSpan)
              else
                if any Modifier.isPrivate mods
                  then Just (StaticPrivateField, sourceSpan)
                  else Just (StaticPackageField, sourceSpan)
      else
        if any Modifier.isPublic mods
          then Just (InstancePublicField, sourceSpan)
          else
            if any Modifier.isProtected mods
              then Just (InstanceProtectedField, sourceSpan)
              else
                if any Modifier.isPrivate mods
                  then Just (InstancePrivateField, sourceSpan)
                  else Just (InstancePackageField, sourceSpan)
  ConstructorDecl sourceSpan _ _ _ _ _ _ -> Just (Constructor, sourceSpan)
  MethodDecl sourceSpan _ _ _ _ _ _ _ _ -> Just (Method, sourceSpan)
  _ -> Nothing

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
  map (createError . snd) (filter (\((rankA, _), (rankB, _)) -> rankA > rankB) (zip declOrder (tail declOrder)))
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
