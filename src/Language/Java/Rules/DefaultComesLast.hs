{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.DefaultComesLast (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  Switch _ _ blocks <- universeBi cUnit
  map (message path) (filter checkForDefault (init blocks))

message :: FilePath -> SwitchBlock Parsed -> RDF.Diagnostic
message path (SwitchBlock span _ _) = RDF.rangeDiagnostic "Language.Java.Rules.DefaultComesLast" "Der Default Fall sollte in einem Switch Case zuletzt definiert werden." span path

checkForDefault :: SwitchBlock Parsed -> Bool
checkForDefault (SwitchBlock _ Default _) = True
checkForDefault (SwitchBlock {}) = False
