import qualified AvoidNegationsTests
import qualified AvoidOuterNegationsTests
import qualified AvoidStarImportTests
import qualified ConsistentOverrideEqualsHashCodeTests
import qualified DeclarationOrderTests
import qualified DefaultComesLastTests
import qualified ExplicitValueTests
import qualified FinalParametersTests
import qualified InitializeVariablesTests
import qualified MethodInvNumberTests
import qualified MethodNamesTests
import qualified ModifiedControlVariableTests
import qualified MultipleStringLiteralsTests
import qualified MultipleVariableDeclarationsTests
import qualified NamingConventionsTests
import qualified NeedBracesTests
import qualified NoAnnotationsTests
import qualified NoCastsTests
import qualified NoDummyNamesTests
import qualified NoFurtherDataStructuresTests
import qualified NoGermanNamesTests
import qualified NoIncDecInExpressionTests
import qualified NoLoopBreakTests
import qualified NoNullPointerExceptionsForControlTests
import qualified ParameterNumberTests
import qualified PreferExpressionsTests
import qualified PrivateAttributesTests
import qualified ReduceScopeTests
import qualified RedundantModifiersTests
import qualified SameExecutionsInIfTests
import qualified SimplifyBooleanTests
import qualified SingleTopLevelClassTests
import System.Log.Logger (Priority (..), setLevel, updateGlobalLogger)
import Test.HUnit
import qualified UseAssignOpTests
import qualified UseElseTests
import qualified UseIncrementDecrementOperatorTests
import qualified UseJavaArrayTypeStyleTests
import qualified UsePostIncrementDecrementTests

main :: IO ()
main = do
  updateGlobalLogger "jlint" (setLevel DEBUG)
  runTestTTAndExit tests

tests :: Test
tests =
  test
    [ "AvoidNegations" ~: AvoidNegationsTests.tests,
      "AvoidOuterNegations" ~: AvoidOuterNegationsTests.tests,
      "AvoidStarImport" ~: AvoidStarImportTests.tests,
      "ConsistentOverrideEqualsHashCode" ~: ConsistentOverrideEqualsHashCodeTests.tests,
      "DeclarationOrder" ~: DeclarationOrderTests.tests,
      "DefaultComesLast" ~: DefaultComesLastTests.tests,
      "ExplicitValue" ~: ExplicitValueTests.tests,
      "FinalParameters" ~: FinalParametersTests.tests,
      "InitialzeVariables" ~: InitializeVariablesTests.tests,
      "MethodInvNumber" ~: MethodInvNumberTests.tests,
      -- "MethodNames" ~: MethodNamesTests.tests,
      "ModifiedControlVariable" ~: ModifiedControlVariableTests.tests,
      "MultipleVariableDeclarations" ~: MultipleVariableDeclarationsTests.tests,
      "MultipleStringLiteralsTests" ~: MultipleStringLiteralsTests.tests,
      "NamingConventions" ~: NamingConventionsTests.tests,
      "NeedBraces" ~: NeedBracesTests.tests,
      "NoAnnotations" ~: NoAnnotationsTests.tests,
      "NoCasts" ~: NoCastsTests.tests,
      "NoDummyNames" ~: NoDummyNamesTests.tests,
      "NoFurtherDataStructures" ~: NoFurtherDataStructuresTests.tests,
      "NoGermanNames" ~: NoGermanNamesTests.tests,
      "NoLoopBreak" ~: NoLoopBreakTests.tests,
      "NoIncDecInExpression" ~: NoIncDecInExpressionTests.tests,
      "NoNullPointerExpceptionsForControl" ~: NoNullPointerExceptionsForControlTests.tests,
      "ParameterNumber" ~: ParameterNumberTests.tests,
      "PreferExpressions" ~: PreferExpressionsTests.tests,
      "PrivateAttributes" ~: PrivateAttributesTests.tests,
      "ReduceScope" ~: ReduceScopeTests.tests,
      "RedundantModifiers" ~: RedundantModifiersTests.tests,
      "SameExecutionsInIf" ~: SameExecutionsInIfTests.tests,
      "SimplifyBoolean" ~: SimplifyBooleanTests.tests,
      "SingleTopLevelClass" ~: SingleTopLevelClassTests.tests,
      "UseAssignOp" ~: UseAssignOpTests.tests,
      "UseElse" ~: UseElseTests.tests,
      "UseIncrementDecrementOperator" ~: UseIncrementDecrementOperatorTests.tests,
      "UseJavaArrayTypeStyle" ~: UseJavaArrayTypeStyleTests.tests,
      "UsePostIncrementDecrement" ~: UsePostIncrementDecrementTests.tests
    ]
