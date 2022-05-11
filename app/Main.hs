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
check unit =
    case unit of
        CompilationUnit _ _ classtype->
            checkType classtype

checkType :: [TypeDecl] -> IO ()
checkType list =
  case list of
    (node:remaininglist) ->
      case node of
        ClassTypeDecl a ->
          do
            checkClassType a
            checkType remaininglist
        InterfaceTypeDecl a ->
            print "Interface"
    [] ->
      print "Empty Node"

checkClassType :: ClassDecl -> IO ()
checkClassType node =
    case node of
      ClassDecl _ _ _ _ _ i ->
        checkClassBody i
      EnumDecl {} ->
        print "Enum"
      
checkClassBody :: ClassBody -> IO ()
checkClassBody node =
    case node of 
        ClassBody d ->
          checkDecl d   

checkDecl :: [Decl] -> IO ()
checkDecl list =
  case list of
    (node:remaininlist) ->
      case node of
        MemberDecl member ->
          do
            checkMemberDecl member
            checkDecl remaininlist
        InitDecl _ _ -> 
          print "InitDecl"
    [] ->
      print "Empty Node"
      
checkMemberDecl :: MemberDecl -> IO ()
checkMemberDecl member =
    case member of
      FieldDecl m t v ->
        checkFieldDecl m t v
      MethodDecl {} ->
        print "Method"
      ConstructorDecl {} ->
        print "Constructor"
      MemberClassDecl _ ->
        print "MemberClass"
      MemberInterfaceDecl _ ->
        print "MemberInterface"


checkFieldDecl modifier vartype varname =
  case modifier of
    [] ->
      print "No Modifier"
    (x:xs) ->
      if x == Public then
        do
          print x
          print varname
          checkFieldDecl xs vartype varname
      else
        checkFieldDecl xs vartype varname

{-
@Lukas
Der Itteriert nun komplett durch die Struktur bis zur Fielddeclaration
bisher sind alle Funktionen eher IO Funktionen und er gibt jeweils nur die Felder aus.
In der Fielddeclaration printet er nun alle public Elemente jeweils aus.
Als nächste Schritte müsste dann von "unten" also von der FieldDecl hoch das in eine Liste verschachteln in der halt
alle Variablen sind. Entweder dann oder davor muss natürlich noch nach dem Modifier public gefiltert werden.
Bisher ist für jede "Ebene" eine eigene Funktion geschrieben.
-}
