{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Rules.DefaultComesLast (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  (Switch _ _ blocks) <- universeBi cUnit
  checkDefaultComesLast blocks path

checkDefaultComesLast :: [SwitchBlock] -> FilePath -> [RDF.Diagnostic]
checkDefaultComesLast blocks path =
  case blocks of
    [] -> mzero
    [_] -> mzero
    (SwitchBlock span label _) : xs ->
      case label of
        Default -> [RDF.rangeDiagnostic "Language.Java.Rules.DefaultComesLast" "Der Default Fall sollte in einem Switch Case zuletzt definiert werden." span path]
        SwitchCase _ -> checkDefaultComesLast xs path
