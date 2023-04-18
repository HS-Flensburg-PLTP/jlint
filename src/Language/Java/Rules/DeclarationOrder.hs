module Language.Java.Rules.DeclarationOrder (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
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
  deriving (Eq, Ord, Show)

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = checkRank path (extractMemberDecls cUnit)

extractMemberDecls :: CompilationUnit -> [(Rank, SourceSpan)]
extractMemberDecls cUnit = do
  toplvlDecl <- universeBi cUnit
  checkTopLvlStmts toplvlDecl

checkTopLvlStmts :: MemberDecl -> [(Rank, SourceSpan)]
checkTopLvlStmts toplvlDecl = case toplvlDecl of
  FieldDecl sourceSpan mods _ _ ->
    if Static `elem` mods
      then
        if Public `elem` mods
          then [(StaticPublicField, sourceSpan)]
          else
            if Protected `elem` mods
              then [(StaticProtectedField, sourceSpan)]
              else
                if Private `elem` mods
                  then [(StaticPrivateField, sourceSpan)]
                  else [(StaticPackageField, sourceSpan)]
      else
        if Public `elem` mods
          then [(InstancePublicField, sourceSpan)]
          else
            if Protected `elem` mods
              then [(InstanceProtectedField, sourceSpan)]
              else
                if Private `elem` mods
                  then [(InstancePrivateField, sourceSpan)]
                  else [(InstancePackageField, sourceSpan)]
  ConstructorDecl sourceSpan _ _ _ _ _ _ -> [(Constructor, sourceSpan)]
  MethodDecl sourceSpan _ _ _ _ _ _ _ _ -> [(Method, sourceSpan)]
  _ -> mzero

checkRank :: FilePath -> [(Rank, SourceSpan)] -> [RDF.Diagnostic]
checkRank path declOrder =
  map
    (\((rank, sourcespan), _) -> RDF.rangeDiagnostic "Language.Java.Rules.DeclarationOrder" (show rank ++ " an der falschen Stelle deklariert.") sourcespan path)
    ( filter
        (\((rankA, _), (rankB, _)) -> rankA > rankB)
        (zip declOrder (tail declOrder))
    )
