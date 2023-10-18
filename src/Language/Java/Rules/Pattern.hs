module Language.Java.Rules.Pattern (check, Pattern (..)) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (unpack)
import Data.Yaml (FromJSON (..), Value (..))
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax hiding (String)
import qualified Language.Java.Syntax.Ident as Ident
import qualified RDF
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Token (TokenParser)
import qualified Text.Parsec.Token as P

data Pattern
  = Method {methodName :: String, args :: [Integer]}
  | Invocation {field :: String, methodName :: String}

lexer :: TokenParser ()
lexer = P.makeTokenParser javaStyle

parens :: Parsec String () a -> Parsec String () a
parens = P.parens lexer

integer :: Parsec String () Integer
integer = P.integer lexer

identifier :: Parsec String () String
identifier = P.identifier lexer

comma :: Parsec String () String
comma = P.comma lexer

dot :: Parsec String () String
dot = P.dot lexer

patternParser :: Parsec String () Pattern
patternParser = do
  ident1 <- identifier
  ( do
      args <- parens (integer `Parsec.sepBy` comma)
      pure (Method ident1 args)
    )
    <|> ( do
            _ <- dot
            ident2 <- identifier
            _ <- parens (return ())
            pure (Invocation ident1 ident2)
        )

instance FromJSON Pattern where
  parseJSON (String text) =
    case Parsec.runParser patternParser () "" (unpack text) of
      Right res -> pure res
      Left err -> fail ("Parsing " ++ unpack text ++ " failed with error " ++ show err)
  parseJSON _ = fail "Pattern should be a string"

check :: Pattern -> String -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check pattern_ explanation cUnit path =
  universeBi cUnit >>= checkExp
  where
    checkExp :: Exp Parsed -> [RDF.Diagnostic]
    checkExp (MethodInv invocation)
      | Just span <- methodCallHasPattern invocation pattern_ =
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.Pattern"
                [ "Es wurde ein unerwünschtes Muster entdeckt.",
                  explanation
                ]
                span
                path
            )
      | otherwise = mzero
    checkExp _ = mzero

methodCallHasPattern :: MethodInvocation Parsed -> Pattern -> Maybe SourceSpan
methodCallHasPattern (MethodCall span _ ident arguments) (Method patternName patternArgs)
  | Ident.name ident == patternName
      && length arguments == length patternArgs
      && and (zipWith isIntLiteral arguments patternArgs) =
      Just span
  | otherwise = Nothing
methodCallHasPattern (PrimaryMethodCall span (This {}) _ ident arguments) (Method patternName patternArgs)
  | Ident.name ident == patternName
      && length arguments == length patternArgs
      && and (zipWith isIntLiteral arguments patternArgs) =
      Just span
  | otherwise = Nothing
methodCallHasPattern
  (MethodCall span (Just (Name _ (fieldIdent :| []))) methodIdent [])
  (Invocation fieldName methodName)
    | Ident.name fieldIdent == fieldName && Ident.name methodIdent == methodName = Just span
    | otherwise = Nothing
methodCallHasPattern
  (PrimaryMethodCall span (FieldAccess (PrimaryFieldAccess _ (This {}) fieldIdent)) _ methodIdent [])
  (Invocation fieldName methodName)
    | Ident.name fieldIdent == fieldName && Ident.name methodIdent == methodName = Just span
    | otherwise = Nothing
methodCallHasPattern _ _ = Nothing

isIntLiteral :: Exp Parsed -> Integer -> Bool
isIntLiteral (Lit (Int _ value1)) value2 = value1 == value2
isIntLiteral _ _ = False
