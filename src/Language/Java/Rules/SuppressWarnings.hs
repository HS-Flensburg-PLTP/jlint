module Language.Java.Rules.SuppressWarnings (check) where

import Control.Monad (guard, mplus, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (Located, sourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Annotation.Extra as Annotation.Extra
import qualified Language.Java.Syntax.ClassDecl as ClassDecl
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import QualifiedIdent (QualifiedIdent (..))
import qualified RDF

check :: [QualifiedIdent] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path =
  (universeBi cUnit >>= checkClassDecl)
    `mplus` (universeBi cUnit >>= checkInterfaceDecl)
    `mplus` (universeBi cUnit >>= checkFormalParam)
    `mplus` (universeBi cUnit >>= checkStmt)
  where
    checkClassDecl :: ClassDecl Parsed -> [RDF.Diagnostic]
    checkClassDecl classDecl =
      ( do
          guard (any isSuppressWarnings (modifiers classDecl))
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SuppressWarnings"
                [ "Die Annotation",
                  Markdown.code "SuppressWarnings",
                  "sollte nicht an die gesamte Klasse geschrieben werden, sondern an eine einzelne Zuweisung."
                ]
                (sourceSpan classDecl)
                path
            )
      )
        `mplus` do
          memberDecl <- universeBi (classDeclDecls classDecl) :: [MemberDecl Parsed]
          checkMemberDecl (map methodName filteredWhitelist) memberDecl
      where
        filteredWhitelist =
          filter (\qualified -> className qualified == Ident.name (ClassDecl.ident classDecl)) whitelist

    checkMemberDecl :: [String] -> MemberDecl Parsed -> [RDF.Diagnostic]
    checkMemberDecl methodNames (MethodDecl span modifiers _ _ ident _ _ _ methodBody)
      | Ident.name ident `elem` methodNames =
          if any isSuppressWarnings modifiers
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.SuppressWarnings"
                    [ "Die Annotation",
                      Markdown.code "SuppressWarnings",
                      "sollte nicht an die gesamte Methode geschrieben werden, sondern an eine einzelne Zuweisung."
                    ]
                    span
                    path
                )
            else mzero
      | otherwise =
          if Ident.name ident `elem` methodNames
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.SuppressWarnings"
                    [ "Die Annotation",
                      Markdown.code "SuppressWarnings",
                      "sollte in dieser Methode nicht verwendet werden."
                    ]
                    span
                    path
                )
                ++ (universeBi methodBody >>= checkBlockStmt)
            else universeBi methodBody >>= checkBlockStmt
    checkMemberDecl _ (FieldDecl span modifiers _ _)
      | any isSuppressWarnings modifiers =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SuppressWarnings"
                [ "Die Annotation",
                  Markdown.code "SuppressWarnings",
                  "sollte nicht verwendet werden."
                ]
                span
                path
            )
      | otherwise = mzero
    checkMemberDecl _ (ConstructorDecl span modifiers _ _ _ _ _)
      | any isSuppressWarnings modifiers =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SuppressWarnings"
                [ "Die Annotation",
                  Markdown.code "SuppressWarnings",
                  "sollte nicht verwendet werden."
                ]
                span
                path
            )
      | otherwise = mzero
    checkMemberDecl _ _ = mzero
    checkInterfaceDecl :: InterfaceDecl Parsed -> [RDF.Diagnostic]
    checkInterfaceDecl = checkModifiers

    checkBlockStmt :: BlockStmt Parsed -> [RDF.Diagnostic]
    checkBlockStmt = checkModifiers

    checkFormalParam :: FormalParam Parsed -> [RDF.Diagnostic]
    checkFormalParam = checkModifiers

    checkStmt :: Stmt Parsed -> [RDF.Diagnostic]
    checkStmt = checkModifiers

    checkModifiers :: (Located (ast p), HasModifiers ast) => ast p -> [RDF.Diagnostic]
    checkModifiers ast
      | any isSuppressWarnings (modifiers ast) =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.SuppressWarnings"
                [ "Die Annotation",
                  Markdown.code "SuppressWarnings",
                  "sollte nicht verwendet werden."
                ]
                (sourceSpan ast)
                path
            )
      | otherwise = mzero

isSuppressWarnings :: Modifier p -> Bool
isSuppressWarnings (Annotation annotation) =
  prettyPrint (Annotation.Extra.name annotation) == "SuppressWarnings"
isSuppressWarnings _ = False

class HasModifiers c where
  modifiers :: c p -> [Modifier p]

instance HasModifiers ClassDecl where
  modifiers (ClassDecl _ modifiers _ _ _ _ _) = modifiers
  modifiers (RecordDecl _ modifiers _ _ _ _ _) = modifiers
  modifiers (EnumDecl _ modifiers _ _ _) = modifiers

instance HasModifiers InterfaceDecl where
  modifiers (InterfaceDecl _ _ modifiers _ _ _ _ _) = modifiers

instance HasModifiers FormalParam where
  modifiers (FormalParam _ modifiers _ _ _) = modifiers

instance HasModifiers BlockStmt where
  modifiers (LocalVars _ modifiers _ _) = modifiers
  modifiers _ = []

instance HasModifiers Stmt where
  modifiers (EnhancedFor _ modifiers _ _ _ _) = modifiers
  modifiers _ = []

classDeclDecls :: ClassDecl p -> [Decl p]
classDeclDecls (ClassDecl _ _ _ _ _ _ (ClassBody decls)) = decls
classDeclDecls (RecordDecl _ _ _ _ _ _ (ClassBody decls)) = decls
classDeclDecls (EnumDecl _ _ _ _ (EnumBody _ decls)) = decls
