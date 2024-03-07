{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.LocalTypeInference where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  LocalVars _ _ type_ (VarDecl _ _ (Just varInit) :| []) :: BlockStmt Parsed <- universeBi cUnit
  if localTypeInferenceUsed type_
    then
      if isParameterLessConstructorWithDiamond varInit
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.LocalTypeInference"
                [ "Hier wird der Diamantoperator",
                  Markdown.code (prettyPrint Diamond),
                  "zusammen mit der lokalen Typinferenz verwendet.",
                  "Dadurch ist der Compiler nicht in der Lage, den Typ, der für den Diamantoperator eingesetzt werden soll, zu ermitteln und verwendet",
                  Markdown.code "Object" ++ ".",
                  "Daher sollte der Diamantoperator",
                  Markdown.code (prettyPrint Diamond),
                  "durch einen konkreten Typ ersetzt werden."
                ]
                (sourceSpan varInit)
                path
            )
        else mzero
    else
      if isNullInit varInit
        then mzero
        else
          if isParameterLessConstructorWithDiamond varInit
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.LocalTypeInference"
                    [ "Die explizite Typ-Angabe",
                      Markdown.code (prettyPrint type_),
                      "kann durch lokale Typinferenz mit",
                      Markdown.code "var",
                      "ersetzt werden. Dabei muss allerdings der Diamantoperator",
                      Markdown.code (prettyPrint Diamond),
                      "durch einen konkreten Typ ersetzt werden."
                    ]
                    (sourceSpan type_)
                    path
                )
            else
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.LocalTypeInference"
                    [ "Die explizite Typ-Angabe",
                      Markdown.code (prettyPrint type_),
                      "kann durch lokale Typinferenz mit",
                      Markdown.code "var",
                      "ersetzt werden."
                    ]
                    (sourceSpan type_)
                    path
                )

isNullInit :: VarInit Parsed -> Bool
isNullInit (InitExp (Lit (Null _))) = True
isNullInit _ = False

localTypeInferenceUsed :: Type -> Bool
localTypeInferenceUsed (RefType (ClassRefType (ClassType _ ((Ident _ "var", []) :| [])))) = True
localTypeInferenceUsed _ = False

isParameterLessConstructorWithDiamond :: VarInit Parsed -> Bool
isParameterLessConstructorWithDiamond (InitExp (InstanceCreation _ _ (TypeDeclSpecifierUnqualifiedWithDiamond _ _) [] _)) = True
isParameterLessConstructorWithDiamond (InitExp (InstanceCreation _ _ (TypeDeclSpecifierWithDiamond {}) [] _)) = True
isParameterLessConstructorWithDiamond _ = False
