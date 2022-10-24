module CheckstyleXML (toRDF) where

import Data.Either (isLeft, rights)
import Data.List (intercalate)
import RDF
  ( Code (..),
    Diagnostic (..),
    Location (..),
    Position (..),
    Range (..),
    Severity (..),
    Source (..),
  )
import Text.Read (readMaybe)
import Text.XML.HaXml
  ( Attribute,
    Content (..),
    Document (..),
    Element (..),
    QName (..),
  )
import Text.XML.HaXml.Escape (stdXmlEscaper, xmlUnEscape)

toRDF :: Show i => Document i -> Either String [Diagnostic]
toRDF (Document _ _ (Elem (N "checkstyle") _ children) _) = do
  let res = map (withCElem (parseFile . xmlUnEscape stdXmlEscaper)) children
  if any isLeft res
    then Left (intercalate ", " (map show res))
    else return (concat (rights res))
toRDF elem = Left ("Expecting checkstyle element but found " ++ show elem)

withCElem :: (Element i -> Either String [a]) -> Content i -> Either String [a]
withCElem f (CElem elem _) = f elem
withCElem _ _ = Right []

parseFile :: Show i => Element i -> Either String [Diagnostic]
parseFile (Elem (N "file") attributes children) = do
  path <- lookupAttr "name" attributes
  let res = map (withCElem (parseError path)) children
  if any isLeft res
    then Left (intercalate ", " (map show res))
    else return (concat (rights res))
parseFile _ = return []

parseError :: Show i => String -> Element i -> Either String [Diagnostic]
parseError path (Elem (N "error") attributes _) = do
  lineStr <- lookupAttr "line" attributes
  line <- readEither lineStr
  columnStr <- lookupAttr "column" attributes
  column <- readEither columnStr
  severityStr <- lookupAttr "severity" attributes
  severity <- severityFromString severityStr
  message <- lookupAttr "message" attributes
  source <- lookupAttr "source" attributes
  return
    [ Diagnostic
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
          source = Just (Source {name = "checkstyle", url = Nothing}),
          code = Just (Code {value = source, codeURL = Nothing}),
          suggestions = Nothing,
          originalOutput = Nothing
        }
    ]
parseError _ _ = return []

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
    [value] -> Right (show value)
    _ -> Left ("No atttribute " ++ name ++ " found in " ++ show attributes)

isName :: String -> QName -> Bool
isName name' (N name) = name == name'
isName _ (QN _ _) = False
