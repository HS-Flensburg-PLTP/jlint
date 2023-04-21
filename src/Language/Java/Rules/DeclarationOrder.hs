module Language.Java.Rules.DeclarationOrder (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (maybeToList)
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

showRank :: Rank -> String
showRank StaticPublicField = "static public Variable"
showRank StaticProtectedField = "static protected Variable"
showRank StaticPackageField = "static package Variable"
showRank StaticPrivateField = "static private Variable"
showRank InstancePublicField = "public Variable"
showRank InstanceProtectedField = "protected Variable"
showRank InstancePackageField = "package Variable"
showRank InstancePrivateField = "private Variable"
showRank Constructor = "Konstruktor"
showRank Method = "Methode"

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkRank path (extractMemberDecls cUnit)

extractMemberDecls :: CompilationUnit -> [(Rank, SourceSpan)]
extractMemberDecls cUnit = do
  toplvlDecl <- universeBi cUnit
  maybeToList (checkTopLvlStmts toplvlDecl)

checkTopLvlStmts :: MemberDecl -> Maybe (Rank, SourceSpan)
checkTopLvlStmts toplvlDecl = case toplvlDecl of
  FieldDecl sourceSpan mods _ _ ->
    if Static `elem` mods
      then
        if checkForPublic mods
          then Just (StaticPublicField, sourceSpan)
          else
            if Protected `elem` mods
              then Just (StaticProtectedField, sourceSpan)
              else
                if Private `elem` mods
                  then Just (StaticPrivateField, sourceSpan)
                  else Just (StaticPackageField, sourceSpan)
      else
        if checkForPublic mods
          then Just (InstancePublicField, sourceSpan)
          else
            if Protected `elem` mods
              then Just (InstanceProtectedField, sourceSpan)
              else
                if Private `elem` mods
                  then Just (InstancePrivateField, sourceSpan)
                  else Just (InstancePackageField, sourceSpan)
  ConstructorDecl sourceSpan _ _ _ _ _ _ -> Just (Constructor, sourceSpan)
  MethodDecl sourceSpan _ _ _ _ _ _ _ _ -> Just (Method, sourceSpan)
  _ -> Nothing

checkForPublic :: [Modifier] -> Bool
checkForPublic [] = False
checkForPublic ((Public _) : _) = True
checkForPublic (_ : xs) = checkForPublic xs

checkRank :: FilePath -> [(Rank, SourceSpan)] -> [RDF.Diagnostic]
checkRank path declOrder =
  map (createError . snd) (filter (\((rankA, _), (rankB, _)) -> rankA > rankB) (zip declOrder (tail declOrder)))
  where
    createError :: (Rank, SourceSpan) -> RDF.Diagnostic
    createError (rank, sourcespan) = RDF.rangeDiagnostic "Language.Java.Rules.DeclarationOrder" (showRank rank ++ " an der falschen Stelle deklariert.") sourcespan path
