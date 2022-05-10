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

-- newType Error 
--   = NonPrivateError String

-- -- find non private Attributes and return them as Error
-- findAttributes :: CompilationUnit -> Maybe (List Error)
-- findAttributes (CompilationUnit _ _ [typeDecl]) =
--   case typeDecl of
--     ClassDecl ->

-- iterateList :: List a -> List b -> (a -> b) -> Maybe (List b) 
-- iterateList inputL resultL check= 
--   case inputL of 
--     [] ->
--       resultL
--     x :: xs ->
--       iterateList xs (check x :: resultL)
      



-- check :: CompilationUnit -> Maybe (List Error)
-- check ((name word):xs) a =
--   print name
--   if word == a then True else check xs a