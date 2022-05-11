module Main where

import Lib
import Options.Applicative
import Data.Semigroup((<>))
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Pretty(prettyPrint, pretty)

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


buildAst path pretty = 
  do 
    input <- readFile path
    let result = parser compilationUnit input
    case result of 
      Left error -> 
        print error
      Right cUnit -> 
        -- if pretty then writeFile "./ast.txt" (prettyPrint cUnit) else print cUnit
        print cUnit

-- @ Philipp use "jlint-exe --path ./test/Strings.java" to execute
-- ignorier mein zeug. kannst du auch rauslöschen sonst.


-- Grundidee: jede Typvariable eines jeden Konstruktor (bsp. CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl])
-- wird in eine liste gewrapped. Filter kann durch diese Listen durchiterrieren und mithilfe einer gegebenen Methode (check) filtern.
-- zurückgegeben wir eine Liste die alle gesuchen Elemente erhält. Diese Methode muss für jeden "node / typenvariable" wieder mit angepasster
-- filtermethode aufgerufen werden, bis wir die gewünschten Nodes haben. 

-- filters list using a filteringfunction. Uses a Akkumulator 

filter :: List a -> (a -> Bool) -> List a
filter inputL check = accFilter inputL [] check
  where
    accFilter inputL resultL check =
      case list of
        [] ->
          resultL
        x :: xs 
            | check x -> iterate xs (x ::resultL) check
            | otherwise -> iterate xs resultL check


-- Work in progress compares if two values are identical 
check :: a -> b -> Bool
check valRef valToCheck =
  valRef == valToCheck

-- data Error 
--   = NonPrivateError String


-- diese Funktion soll später genutz werden um filter immer wieder mit entsprechender Filterfunktion (check) aufzurufen.
-- damit diese Funktion weiß wie sie durch den ast navigieren soll bzw. nach welchen Nodes / Name sie filter soll, könnte 
-- mann ihr eine Art Pfad übergeben. Dies Pfadangabe könnte beispielsweise inform einer Partielle Funktion geschen, wobei 
-- das jeweils nächste Argument definiert, nach welchem Node als nächstes gefilter werden soll. Ist nur eine Idee.
-- Keine Ahnung ob das umsetzbar ist. Ist jetzt eh noch egal für den ersten Implementierungsansatz. 

-- -- find non private Attributes and return them as Error
-- findAttributes :: CompilationUnit -> List Error
-- findAttributes 
