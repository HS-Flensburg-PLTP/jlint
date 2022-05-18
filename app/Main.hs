module Main where

import Lib
import Options.Applicative
import Data.Semigroup((<>))
import Language.Java.Parser (parser, compilationUnit, modifier)
import Language.Java.Pretty(prettyPrint, pretty)
import Language.Java.Syntax

main :: IO ()
main = importJava =<< execParser opts
  where
    opts = info (params <**> helper)
      ( fullDesc
     <> progDesc "Parse the Java-File"
     <> header "Linter for Java-Code" )


importJava :: Params -> IO ()
importJava (Params path prettie) = buildAst path prettie

data Params = Params
  {
    path       :: String
  , prettie    :: Bool }


params :: Parser Params
params = Params
      <$> strOption
          ( long "path"
          <> metavar "SRCPath"
          <> help "Path to the Java-File")
      <*> switch
          ( long "prettie"
          <> help "By setting these Param the AST is showing after the Prettier")

buildAst :: FilePath -> Bool -> IO ()
buildAst path pretty =
  do
    input <- readFile path
    let result = parser compilationUnit input
    case result of
      Left error ->
        print error
      Right cUnit ->
        -- if pretty then writeFile "./ast.txt" (prettyPrint cUnit) else print cUnit
        check cUnit

check :: CompilationUnit -> IO ()
check (CompilationUnit _ _ classtype) =
    do
      print (checkType classtype)

data Errors = ClassVarNotPrivate {var :: String}
  deriving (Show)

checkType :: [TypeDecl] -> [Errors]
checkType [] = []
checkType (typedecl:restlist) = case typedecl of
  ClassTypeDecl cd -> checkClassType cd ++ checkType restlist
  InterfaceTypeDecl id -> []


checkClassType :: ClassDecl -> [Errors]
checkClassType node =
    case node of
      ClassDecl _ _ _ _ _ (ClassBody body) ->
        checkDecl body
      EnumDecl {} -> []


checkDecl :: [Decl] -> [Errors]
checkDecl [] = []
checkDecl (member:restlist) = case member of
  MemberDecl md -> checkMemberDecl md ++ checkDecl restlist
  InitDecl b bl -> []

checkMemberDecl :: MemberDecl -> [Errors]
checkMemberDecl member = 
  case member of
    FieldDecl mods ty ((VarDecl (VarId (Ident n)) x) : xs) -> checkFieldDecl mods n
    MethodDecl mods tps m_ty id fps rts m_exp mb -> []
    ConstructorDecl mods tps id fps rts cb -> []
    MemberClassDecl cd -> []
    MemberInterfaceDecl id -> []


checkFieldDecl :: [Modifier] -> String -> [Errors]
checkFieldDecl [] varname = [ClassVarNotPrivate {var = varname}]
checkFieldDecl modifier varname = [ClassVarNotPrivate {var = varname} | Private `notElem` modifier]
