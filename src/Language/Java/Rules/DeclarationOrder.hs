module Language.Java.Rules.DeclarationOrder (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (maybeToList)
import qualified Language.Java.HelperMethods.Modifier as Modifier
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified RDF

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
rankToString StaticPublicField = "static public Variable"
rankToString StaticProtectedField = "static protected Variable"
rankToString StaticPackageField = "static package Variable"
rankToString StaticPrivateField = "static private Variable"
rankToString InstancePublicField = "public Variable"
rankToString InstanceProtectedField = "protected Variable"
rankToString InstancePackageField = "package Variable"
rankToString InstancePrivateField = "private Variable"
rankToString Constructor = "Konstruktor"
rankToString Method = "Methode"

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkRank path (extractMemberDecls cUnit)

extractMemberDecls :: CompilationUnit Parsed -> [(Rank, SourceSpan)]
extractMemberDecls cUnit = do
  toplvlDecl <- universeBi cUnit
  maybeToList (checkTopLvlStmts toplvlDecl)

checkTopLvlStmts :: MemberDecl Parsed -> Maybe (Rank, SourceSpan)
checkTopLvlStmts toplvlDecl = case toplvlDecl of
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

checkRank :: FilePath -> [(Rank, SourceSpan)] -> [RDF.Diagnostic]
checkRank path declOrder =
  map (createError . snd) (filter (\((rankA, _), (rankB, _)) -> rankA > rankB) (zip declOrder (tail declOrder)))
  where
    createError :: (Rank, SourceSpan) -> RDF.Diagnostic
    createError (rank, sourcespan) = RDF.rangeDiagnostic "Language.Java.Rules.DeclarationOrder" (rankToString rank ++ " an der falschen Stelle deklariert.") sourcespan path
