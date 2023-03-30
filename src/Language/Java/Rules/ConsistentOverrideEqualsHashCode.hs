module Language.Java.ConsistentOverrideEqualsHashCode where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.AST (extractMethods)
import Language.Java.Syntax
import RDF (Diagnostic)
import qualified RDF

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkConsistentOverride methods path

checkConsistentOverride :: t0 -> FilePath -> [Diagnostic]
checkConsistentOverride = do 
