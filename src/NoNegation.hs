module NoNegation where

import AST (extractMethods)
import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import RDF (Diagnostic (..), methodDiagnostic)

check :: CompilationUnit -> FilePath -> [Diagnostic]
check cUnit path = do
  method <- extractMethods cUnit
  checkIfThenElseStatements method path (concatFittingMethodAndClassVars (extractMethodVars method) (extractClassVars cUnit))

checkIfThenElseStatements :: (String, MethodBody) -> FilePath -> [(String, Bool)] -> [Diagnostic]
checkIfThenElseStatements (methodName, methodBody) path varList = do
  stmt <- universeBi methodBody
  checkStatement stmt
  where
    checkStatement (IfThenElse (BinOp _ NotEq _) (StmtBlock _) (StmtBlock _)) = return (methodDiagnostic methodName "A negated if-else expression can be used without negation." path)
    checkStatement (IfThenElse (PreNot (Lit (Boolean bool))) (StmtBlock _) (StmtBlock _)) = return (methodDiagnostic methodName ("A negated if-else expression (!" ++ show bool ++ ") can be used without negation.") path)
    checkStatement (IfThenElse (PreNot (ExpName (Name (Ident name : _)))) (StmtBlock _) (StmtBlock _)) =
      if (name, True) `elem` varList then return (methodDiagnostic methodName ("A negated if-else expression (!" ++ name ++ ") can be used without negation.") path) else mzero
    checkStatement _ = mzero

extractClassVars :: CompilationUnit -> [(String, Bool)]
extractClassVars cUnit = do
  var <- universeBi cUnit
  extractNameAndType var
  where
    extractNameAndType (FieldDecl _ _ (PrimType BooleanT) var) = map (\(VarDecl v _) -> extractVarName v True) var
    extractNameAndType (FieldDecl _ _ _ var) = map (\(VarDecl v _) -> extractVarName v False) var
    extractNameAndType _ = mzero

extractVarName :: VarDeclId -> Bool -> (String, Bool)
extractVarName (VarId (Ident varName)) b = (varName, b)
extractVarName (VarDeclArray varDecl) b = extractVarName varDecl b

extractMethodVars :: (String, MethodBody) -> [(String, Bool)]
extractMethodVars (_, methodBody) = do
  var <- universeBi methodBody
  extractNameAndBool var
  where
    extractNameAndBool (LocalVars [] (PrimType BooleanT) var) = map (\(VarDecl v _) -> extractVarName v True) var
    extractNameAndBool (LocalVars [] _ var) = map (\(VarDecl v _) -> extractVarName v False) var
    extractNameAndBool _ = mzero

concatFittingMethodAndClassVars :: [(String, Bool)] -> [(String, Bool)] -> [(String, Bool)]
concatFittingMethodAndClassVars methodVars classVars = methodVars ++ filter (\(name, _) -> (name, True) `notElem` methodVars && (name, False) `notElem` methodVars) classVars
