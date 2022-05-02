{-
Leider wirft dies Programm zum jetzigen Zeitpunkt nur Fehler auf. Es fehlt immer --hello TARGET, was ich aber auch nicht behoben bekomme.
Auch --help hilft nicht. Immer wieder der gleiche Fehler. 
-}

module Main where

import Lib
import Options.Applicative
import Data.Semigroup((<>))

main :: IO ()
{- =<< ist wohl wieder eine Monadfunktion. Habe es so verstanden, dass es für die Sortierung der 
 Inputs notwendig ist. Somit ist eine Eingabe, egal in welcher Reihenfolge, möglich. 

Die execParser-Funktion kümmert sich hierbei um alles, inclusive entgegennehmen der Argumente aus der Kommandozeile, 
anzeigen von Fehlern und Hilfescreen und das Beenden mit einem angemessenen Exit Code
 -}
main = greet =<< execParser opts
  where
    -- Dies ist eine Art Dummy Help Option, welche den Hilfetext angzeigt. 
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )


{-
greet bekommt eine Sample-Instanz übergeben und baut daraus einen String, welcher auf der Konsole ausgegeben wird.
Wird kein passender Sample übergeben, sondern etwas anders, wird einfach nur returnt. 
-}
greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

{-
Die Bedeutung von <$> und <*> sind noch nicht geläufig. Ich vermute derzeit, dass es eventuell Notwendig und Optional ist, da es als Fehler auch immer nur heißt,
dass --hello TARGET fehlt. Des weiteren hat "enthusiasm" ein value (Standardwert) und quiet wird in greet mit False belegt. 

Hierbei müsste es sich um einen Regular options Builder handeln. 
-}
sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )
