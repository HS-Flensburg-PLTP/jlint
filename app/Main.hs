module Main where

import Lib
import Options.Applicative
import Data.Semigroup((<>))
import Language.Java.Parser (parser, compilationUnit)
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

check unit =
    case unit of
        CompilationUnit _ _ classtype->
            checkType classtype

checkType (node:remaininglist) =
    case node of
      ClassTypeDecl a ->
        checkClassType a
      InterfaceTypeDecl a ->
        print "Interface"

checkClassType node =
    case node of
      ClassDecl _ _ _ _ _ i ->
        checkClassBody i

checkClassBody node =
    case node of 
        ClassBody d ->
          checkDecl d   

checkDecl (node:remaining) =
    case node of
      MemberDecl member ->
        checkMemberDecl member
      InitDecl _ _ -> 
        print "InitDecl"

checkMemberDecl member =
    case member of
      FieldDecl m t v ->
        checkFieldDecl m t v
      MethodDecl _ _ _ _ _ _ _ _ ->
        print "Method"
      ConstructorDecl _ _ _ _ _ _ ->
        print "Constructor"
      MemberClassDecl _ ->
        print "MemberClass"
      MemberInterfaceDecl _ ->
        print "MemberInterface"

checkFieldDecl modifier vartype varname =
  do
    print modifier
    print vartype
    print varname

{-
@Lukas
Der Itteriert nun komplett durch die Struktur bis zur Fielddeclaration
bisher sind alle Funktionen eher IO Funktionen und er gibt jeweils nur die Felder aus
Als nächste Schritte müsste dann von "unten" also von der FieldDecl hoch das in eine Liste verschachteln in der halt
alle Variablen sind. Entweder dann oder davor muss natürlich noch nach dem Modifier public gefiltert werden.
Bisher ist für jede "Ebene" eine eigene Funktion geschrieben.
-}
