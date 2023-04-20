import qualified AvoidMultipleTopLevelDeclTests
import qualified AvoidMultipleVarDeclTests
import qualified AvoidNegationsTests
import qualified AvoidStarImportTests
import qualified CheckNonFinalMethodAttributesTests
import qualified CheckNonPrivateAttributesTests
import qualified ConsistentOverrideEqualsHashCodeTests
import qualified DeclarationOrderTests
import qualified DefaultComesLastTests
import qualified EmptyLoopBodyTests
import qualified InitializeVariablesTests
import qualified NamingConventionsTests
import qualified NeedBracesTests
import qualified NoLoopBreakTests
import qualified NoNullPointerExceptionsForControlTests
import qualified ParameterNumberTests
import qualified PreferExpressionsTests
import qualified ReduceScopeTests
import qualified RedundantModifiersTests
import qualified SameExecutionsInIfTests
import qualified SimplifyBooleanReturnTests
import Test.HUnit
import qualified UseAssignOpTests
import qualified UseElseTests
import qualified UseIncrementDecrementOperatorTests
import qualified UseJavaArrayTypeStyleTests

main :: IO ()
main = do
  runTestTTAndExit tests

tests :: Test
tests =
  test
    [ "AvoidMultipleTopLevelDecl" ~: AvoidMultipleTopLevelDeclTests.tests,
      "AvoidMultipleVarDecl" ~: AvoidMultipleVarDeclTests.tests,
      "AvoidNegations" ~: AvoidNegationsTests.tests,
      "AvoidStarImport" ~: AvoidStarImportTests.tests,
      "CheckNonFinalMethodAttributes" ~: CheckNonFinalMethodAttributesTests.tests,
      "CheckNonPrivateAttributes" ~: CheckNonPrivateAttributesTests.tests,
      "ConsistentOverrideEqualsHashCode" ~: ConsistentOverrideEqualsHashCodeTests.tests,
      "DefaultComesLast" ~: DefaultComesLastTests.tests,
      "EmptyLoopBody" ~: EmptyLoopBodyTests.tests,
      "InitialzeVariables" ~: InitializeVariablesTests.tests,
      "NamingConventions" ~: NamingConventionsTests.tests,
      "NeedBraces" ~: NeedBracesTests.tests,
      "NoLoopBreak" ~: NoLoopBreakTests.tests,
      "NoNullPointerExpceptionsForControl" ~: NoNullPointerExceptionsForControlTests.tests,
      "ParameterNumber" ~: ParameterNumberTests.tests,
      "PreferExpressions" ~: PreferExpressionsTests.tests,
      "ReduceScope" ~: ReduceScopeTests.tests,
      "RedundantModifiers" ~: RedundantModifiersTests.tests,
      "SameExecutionsInIf" ~: SameExecutionsInIfTests.tests,
      "SimplifyBooleanReturn" ~: SimplifyBooleanReturnTests.tests,
      "UseAssignOp" ~: UseAssignOpTests.tests,
      "UseElse" ~: UseElseTests.tests,
      "UseIncrementDecrementOperator" ~: UseIncrementDecrementOperatorTests.tests,
      "UseJavaArrayTypeStyle" ~: UseJavaArrayTypeStyleTests.tests,
      "DeclarationOrder" ~: DeclarationOrderTests.tests
    ]
