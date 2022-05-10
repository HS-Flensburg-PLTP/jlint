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
      Left error -> print error
      Right cUnit -> 
        if pretty then writeFile "./ast.txt" (prettyPrint cUnit) else print cUnit
