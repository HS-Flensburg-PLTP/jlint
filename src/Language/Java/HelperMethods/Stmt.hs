module Language.Java.HelperMethods.Stmt
  ( isLoop,
  )
where

import Language.Java.Syntax (Stmt)
import qualified Language.Java.Syntax.Stmt as Stmt

isLoop :: Stmt p -> Bool
isLoop stmt = Stmt.isWhile stmt || Stmt.isDo stmt || Stmt.isBasicFor stmt || Stmt.isEnhancedFor stmt
