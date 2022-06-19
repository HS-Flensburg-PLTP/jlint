{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultComesLast (check) where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), simpleDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  methods <- extractMethods cUnit
  checkDefaultComesLast methods path

checkDefaultComesLast :: (String, MethodBody) -> FilePath -> [Diagnostic]
checkDefaultComesLast (methodName, methodBody) path = do
  stmt <- universeBi methodBody
  checkDefaultComesLast stmt 
  
  where
    checkDefaultComesLast (Switch _ blocks) =
       checkDefaultHelp blocks mzero

    checkDefaultComesLast _ = mzero

    checkDefaultHelp blocks diagnostisList =
      case blocks of
        [] -> 
          diagnostisList
        (SwitchBlock (Default) _) : xs@([SwitchBlock _ _]) ->
          checkDefaultHelp xs ((simpleDiagnostic (msg "'default-case'") path) : diagnostisList)
        _ : xs -> 
          checkDefaultHelp xs diagnostisList
      
    msg t = 
      t ++ " of 'switch-case' in function " ++ methodName ++ " is not defined last."