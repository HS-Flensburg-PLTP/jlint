module CheckstyleXML (toRDF) where

import Data.Either (isLeft, lefts, rights)
import Data.List (intercalate)
import GHC.Base (absentErr)
import RDF (Diagnostic (..), Location (..), Position (..), Range (..), Severity (..))
import qualified RDF
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Text.XML.HaXml
  ( AttValue (..),
    Attribute (..),
    Content (..),
    Document (..),
    Element (..),
    QName (..),
  )
import qualified Text.XML.HaXml as XML
import Text.XML.HaXml.Parse (xmlParse)

toRDF :: Show i => Document i -> Either String [Diagnostic]
toRDF (Document _ _ (Elem (N "checkstyle") _ children) _) = do
  let res = map (withCElem parseFile) children
  if all isLeft res
    then Left (intercalate ", " (lefts res))
    else return (concat (rights res))
toRDF elem = Left ("Expecting checkstyle element but found " ++ show elem)

withCElem :: (Element i -> Either String a) -> Content i -> Either String a
withCElem f (CElem elem _) = f elem
withCElem _ _ = Left "Expecting CElem"

parseFile :: Show i => Element i -> Either String [Diagnostic]
parseFile (Elem (N "file") attributes children) = do
  path <- lookupAttr "name" attributes
  let res = map (withCElem (parseError path)) children
  if all isLeft res
    then Left (intercalate ", " (lefts res))
    else return (rights res)
parseFile elem = Left ("Expecting file element but found " ++ show elem)

parseError :: Show i => String -> Element i -> Either String Diagnostic
parseError path (Elem (N "error") attributes []) = do
  lineStr <- lookupAttr "line" attributes
  line <- readEither lineStr
  columnStr <- lookupAttr "column" attributes
  column <- readEither columnStr
  severityStr <- lookupAttr "severity" attributes
  severity <- severityFromString severityStr
  message <- lookupAttr "message" attributes
  return
    ( Diagnostic
        { message = message,
          location =
            Location
              { path = path,
                range =
                  Just
                    ( Range
                        { start = Position {line = line, column = column},
                          end = Nothing
                        }
                    )
              },
          severity = severity,
          source = Nothing,
          code = Nothing,
          suggestions = Nothing,
          originalOutput = Nothing
        }
    )
parseError _ elem = Left ("Expecting error element but found " ++ show elem)

readEither :: Read a => String -> Either String a
readEither str =
  case readMaybe str of
    Just r -> Right r
    Nothing -> Left ("Expecting int value but found " ++ str)

severityFromString :: String -> Either String Severity
severityFromString "info" = Right INFO
severityFromString "warning" = Right WARNING
severityFromString "error" = Right ERROR
severityFromString str = Left (str ++ " is not a valid severity")

lookupAttr :: String -> [Attribute] -> Either String String
lookupAttr name attributes =
  case map snd (filter (isName name . fst) attributes) of
    [AttValue [Left value]] -> Right value
    _ -> Left ("No atttribute " ++ name ++ " found")

isName :: String -> QName -> Bool
isName name' (N name) = name == name'
isName _ (QN _ _) = False
