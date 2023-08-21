module Language.Java.HelperMethods.Exp (isDataCreation, isPostIncDec) where

import Language.Java.Syntax (Exp (..))
import qualified Language.Java.Syntax.Exp as Exp

isPostIncDec :: Exp p -> Bool
isPostIncDec exp = Exp.isPostInc exp || Exp.isPostDec exp

isDataCreation :: Exp p -> Bool
isDataCreation exp = Exp.isArrayCreate exp || Exp.isIncstanceCreation exp || Exp.isQualInstanceCreation exp
