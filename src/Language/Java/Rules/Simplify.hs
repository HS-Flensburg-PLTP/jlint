module Language.Java.Rules.Simplify (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mzero

-- mapMaybe checkExp (universeBi cUnit)
-- where
--   checkExp :: Exp Parsed -> Maybe RDF.Diagnostic
--   checkExp (BinOp span exp1 Equal exp2)
--     | callsCompareTo exp1 && isZero exp2 || isZero exp1 && callsCompareTo exp2 =
--         return
--           ( RDF.rangeDiagnostic
--               "Language.Java.Rules.Simplify"
--               [ "Statt das Ergebnis von",
--                 Markdown.code "compareTo",
--                 "mit",
--                 Markdown.code "0",
--                 "zu vergleichen, sollte die Methode",
--                 Markdown.code "equals",
--                 "verwendet werden."
--               ]
--               span
--               path
--           )
--     | otherwise = mzero
--   checkExp _ = mzero

-- callsCompareTo :: Exp p -> Bool
-- callsCompareTo (MethodInv (PrimaryMethodCall _ _ _ ident _)) = Ident.name ident == "compareTo"
-- callsCompareTo _ = False

-- isZero :: Exp p -> Bool
-- isZero (Lit (Int _ 0)) = True
-- isZero _ = False
