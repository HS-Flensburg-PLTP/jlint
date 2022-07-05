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

checks :: [CompilationUnit -> FilePath -> [Diagnostic]]
checks =
  [ NeedBraces.check,
    CheckNonFinalMethodAttributes.check,
    CheckNonPrivateAttributes.check,
    EmptyLoopBody.check,
    SimplifyBooleanReturn.check,
    NeedBraces.check
  ]

checkAll :: CompilationUnit -> FilePath -> [Diagnostic]
checkAll cUnit path = concatMap (\f -> f cUnit path) checks
