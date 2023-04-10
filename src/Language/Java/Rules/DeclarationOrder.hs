module Language.Java.Rules.DeclarationOrder (check) where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax (CompilationUnit, MemberDecl (ConstructorDecl, FieldDecl, MethodDecl), Modifier (Private, Protected, Public, Static))
import RDF (Diagnostic)
import qualified RDF

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = checkRank 0 (extractMemberDecls cUnit path)

extractMemberDecls :: CompilationUnit -> FilePath -> [(Int, RDF.Diagnostic)]
extractMemberDecls cUnit path = do
  toplvlDecl <- universeBi cUnit
  checkTopLvlStmts toplvlDecl path

checkTopLvlStmts :: MemberDecl -> FilePath -> [(Int, RDF.Diagnostic)]
checkTopLvlStmts toplvlDecl path = case toplvlDecl of
  FieldDecl sourceSpan mods _ _ ->
    if Static `elem` mods
      then
        if Public `elem` mods
          then [(0, RDF.rangeDiagnostic "DeclarationOrder" "Public Static Variable an der falschen Stelle deklariert." sourceSpan path)]
          else
            if Protected `elem` mods
              then [(1, RDF.rangeDiagnostic "DeclarationOrder" "Protected Static Variable an der falschen Stelle deklariert." sourceSpan path)]
              else
                if Private `elem` mods
                  then [(3, RDF.rangeDiagnostic "DeclarationOrder" "Private Static Variable an der falschen Stelle deklariert." sourceSpan path)]
                  else [(2, RDF.rangeDiagnostic "DeclarationOrder" "Package Static Variable an der falschen Stelle deklariert." sourceSpan path)]
      else
        if Public `elem` mods
          then [(4, RDF.rangeDiagnostic "DeclarationOrder" "Public Instanz Variable an der falschen Stelle deklariert." sourceSpan path)]
          else
            if Protected `elem` mods
              then [(5, RDF.rangeDiagnostic "DeclarationOrder" "Protected Instanz Variable an der falschen Stelle deklariert." sourceSpan path)]
              else
                if Private `elem` mods
                  then [(7, RDF.rangeDiagnostic "DeclarationOrder" "Private Instanz Variable an der falschen Stelle deklariert." sourceSpan path)]
                  else [(6, RDF.rangeDiagnostic "DeclarationOrder" "Package Instanz Variable an der falschen Stelle deklariert." sourceSpan path)]
  ConstructorDecl sourceSpan _ _ _ _ _ _ -> [(8, RDF.rangeDiagnostic "DeclarationOrder" "Konstruktor an der falschen Stelle deklariert." sourceSpan path)]
  MethodDecl sourceSpan _ _ _ _ _ _ _ _ -> [(9, RDF.rangeDiagnostic "DeclarationOrder" "Methode an der falschen Stelle deklariert." sourceSpan path)]
  _ -> mzero

checkRank :: Int -> [(Int, Diagnostic)] -> [Diagnostic]
checkRank allowedrank list =
  case list of
    (rank, diag) : xs ->
      if rank < allowedrank
        then return diag
        else checkRank rank xs
    [] -> mzero
