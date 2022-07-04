module Rules where

import CheckNonFinalMethodAttributes
import CheckNonPrivateAttributes
import EmptyLoopBody
import Language.Java.Syntax
import NeedBraces
import RDF
import SimplifyBooleanReturn

{- possible rules

    NeedBraces.check
    EmptyLoopBody.check
    CheckNonPrivateAttributes.check
    CheckNonFinalMethodAttributes.check
    SimplifyBooleanReturn.check

-}

methods =
  [ NeedBraces.check,
    CheckNonFinalMethodAttributes.check,
    CheckNonPrivateAttributes.check,
    EmptyLoopBody.check,
    SimplifyBooleanReturn.check,
    NeedBraces.check
  ]

rules :: CompilationUnit -> FilePath -> [Diagnostic]
rules cUnit path = concatMap (\x -> x cUnit path) methods
