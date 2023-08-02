import qualified AvoidMultipleTopLevelDeclTests
import qualified AvoidMultipleVarDeclTests
import qualified AvoidNegationsTests
import qualified AvoidOuterNegationsTests
import qualified AvoidStarImportTests
import qualified CheckNonFinalMethodAttributesTests
import qualified CheckNonPrivateAttributesTests
import qualified ConsistentOverrideEqualsHashCodeTests
import qualified DeclarationOrderTests
import qualified DefaultComesLastTests
import qualified ExplicitValueTests
import qualified InitializeVariablesTests
import qualified ModifiedControlVariableTests
import qualified NamingConventionsTests
import qualified NeedBracesTests
import qualified NoFurtherDataStructuresTests as NoFurtherDataStructures
import qualified NoLoopBreakTests
import qualified NoNullPointerExceptionsForControlTests
import qualified ParameterNumberTests
import qualified PreferExpressionsTests
import qualified ProhibitAnnotationsTests
import qualified ProhibitMyIdentPrefixTests
import qualified ReduceScopeTests
import qualified RedundantModifiersTests
import qualified SameExecutionsInIfTests
import qualified SimplifyBooleanReturnTests
import Test.HUnit
import qualified UseAssignOpTests
import qualified UseElseTests
import qualified UseIncrementDecrementOperatorTests
import qualified UseJavaArrayTypeStyleTests
import qualified UsePostIncrementDecrementTests

main :: IO ()
main = do
  runTestTTAndExit tests

tests :: Test
tests =
  test
    [ "AvoidMultipleTopLevelDecl" ~: AvoidMultipleTopLevelDeclTests.tests,
      "AvoidMultipleVarDecl" ~: AvoidMultipleVarDeclTests.tests,
      "AvoidNegations" ~: AvoidNegationsTests.tests,
      "AvoidOuterNegations" ~: AvoidOuterNegationsTests.tests,
      "AvoidStarImport" ~: AvoidStarImportTests.tests,
      "CheckNonFinalMethodAttributes" ~: CheckNonFinalMethodAttributesTests.tests,
      "CheckNonPrivateAttributes" ~: CheckNonPrivateAttributesTests.tests,
      "ConsistentOverrideEqualsHashCode" ~: ConsistentOverrideEqualsHashCodeTests.tests,
      "DefaultComesLast" ~: DefaultComesLastTests.tests,
      "ExplicitValue" ~: ExplicitValueTests.tests,
      "InitialzeVariables" ~: InitializeVariablesTests.tests,
      "ModifiedControlVariable" ~: ModifiedControlVariableTests.tests,
      "NamingConventions" ~: NamingConventionsTests.tests,
      "NeedBraces" ~: NeedBracesTests.tests,
      "NoFurtherDataStructures" ~: NoFurtherDataStructures.tests,
      "NoLoopBreak" ~: NoLoopBreakTests.tests,
      "NoNullPointerExpceptionsForControl" ~: NoNullPointerExceptionsForControlTests.tests,
      "ParameterNumber" ~: ParameterNumberTests.tests,
      "PreferExpressions" ~: PreferExpressionsTests.tests,
      "ProhibitAnnotations" ~: ProhibitAnnotationsTests.tests,
      "ProhibitMyIdentPrefix" ~: ProhibitMyIdentPrefixTests.tests,
      "ReduceScope" ~: ReduceScopeTests.tests,
      "RedundantModifiers" ~: RedundantModifiersTests.tests,
      "SameExecutionsInIf" ~: SameExecutionsInIfTests.tests,
      "SimplifyBooleanReturn" ~: SimplifyBooleanReturnTests.tests,
      "UseAssignOp" ~: UseAssignOpTests.tests,
      "UseElse" ~: UseElseTests.tests,
      "UseIncrementDecrementOperator" ~: UseIncrementDecrementOperatorTests.tests,
      "UseJavaArrayTypeStyle" ~: UseJavaArrayTypeStyleTests.tests,
      "UsePostIncrementDecrement" ~: UsePostIncrementDecrementTests.tests,
      "DeclarationOrder" ~: DeclarationOrderTests.tests
    ]
