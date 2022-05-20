module Main where

import CheckNonFinalMethodAttributes (check)
import Data.Semigroup ((<>))
import Language.Java.Pretty (pretty, prettyPrint)
import Language.Java.Parser (parser, compilationUnit, modifier)
import Language.Java.Syntax
import Lib
import Options.Applicative
import CheckNonPrivateAttributes(check)

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
importJava (Params path pretty) = parseJava path pretty

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

parseJava :: FilePath -> Bool -> IO ()
parseJava path pretty =
  do
    input <- readFile path
    let result = parser compilationUnit input
    case result of
      Left error -> print error
      Right cUnit -> do
        if pretty then print (prettyPrint cUnit) else print cUnit
        print (check cUnit)

