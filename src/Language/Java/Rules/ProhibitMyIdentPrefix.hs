{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.ProhibitMyIdentPrefix where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data
import Language.Java.Syntax
import qualified RDF
import Text.RE.TDFA.String

data MyType a b c d e
  = A a
  | B b
  | C c
  | D d
  | E e

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  thing <- universeBi cUnit
  concatMap ()

{-
  (class, package, method, blaaaa) <- universeBi cUnit
  checkIdent ident path
-}
checkIdent :: Ident -> FilePath -> [RDF.Diagnostic]
checkIdent (Ident ident) path
  | matched (ident ?=~ [re|^([Mm]y)|MY|]) = return (RDF.rangeDiagnostic "Language.Java.Rules.ProhibitMyIdentPrefix" ("Nicht erlaubtes 'My'-Prefix gefunden: " ++ ident) dummySourceSpan path)
  | otherwise = mzero

checkClass :: ClassDecl -> [RDF.Diagnostic]
checkClass classdec = mzero

{-
checkPackageDeclName
checkClassDeclIdent
checkEnumDeclIdent
checkInterfaceDeclIdent
checkMemberDecl
-}
{-
VarDecl Enum
Methoden
Class Interface usw
Package

-}

{-
Packagedecl name
Typedecl
    classdecl ident
    enumdecl
    interface int
decl
    memberdecl

extractIdent dingdasidenthat =

-}
