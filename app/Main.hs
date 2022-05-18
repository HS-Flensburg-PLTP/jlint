module Main where

import Data.Semigroup ((<>))
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Syntax
import Lib
import Options.Applicative

main :: IO ()
main = execParser opts >>= importJava
  where
    opts =
      info
        (params <**> helper)
        ( fullDesc
            <> progDesc "Parse the java file"
            <> header "Linter for java code"
        )

importJava :: Params -> IO ()
importJava (Params path pretty) = buildAst path pretty

data Params = Params
  { path :: String,
    pretty :: Bool
  }

params :: Parser Params
params =
  Params
    <$> strOption
      ( long "path"
          <> metavar "SRCPath"
          <> help "Path to the java file"
      )
    <*> switch
      ( long "pretty"
          <> help "By setting this Parameter the java source representation of the AST is shown"
      )

buildAst :: String -> Bool -> IO ()
buildAst path pretty =
  do
    input <- readFile path
    let result = parser compilationUnit input
    case result of
      Left error -> print error
      Right cUnit -> do
        if pretty then print (prettyPrint cUnit) else print cUnit
        checkCU cUnit

checkCU (CompilationUnit a b c) = do
  print (typeDeclFunc c)

data Errors = FuncVarNotFinal {func :: String, var :: String}
  deriving (Show)

typeDeclFunc :: [TypeDecl] -> [Errors]
typeDeclFunc [] = []
typeDeclFunc (item : restList) =
  case item of
    ClassTypeDecl a -> classTypeDeclFunc a ++ typeDeclFunc restList
    InterfaceTypeDecl a -> []

classTypeDeclFunc item =
  case item of
    ClassDecl modifier (Ident ident) typeParam mRefType refType (ClassBody body) -> classBodyFunc body
    EnumDecl modifier (Ident ident) refType body -> []

classBodyFunc [] = []
classBodyFunc (x : xs) =
  case x of
    MemberDecl memberDecl -> memberDeclFunc memberDecl ++ classBodyFunc xs
    InitDecl bool block -> []

memberDeclFunc item =
  case item of
    MethodDecl modifier typeParam mType (Ident ident) formalParam exceptionType mExp methodBody -> formalParamFunc formalParam ident
    _ -> []

formalParamFunc :: [FormalParam] -> String -> [Errors]
formalParamFunc [] fName = []
formalParamFunc ((FormalParam modifier jType bool (VarId (Ident n))) : xs) fName = ruleTwo modifier fName n ++ (formalParamFunc xs fName)

ruleTwo :: [Modifier] -> String -> String -> [Errors]
ruleTwo [] fName vName = [FuncVarNotFinal {func = fName, var = vName}]
ruleTwo modifier fName vName = if Final `elem` modifier then [] else [FuncVarNotFinal {func = fName, var = vName}]
