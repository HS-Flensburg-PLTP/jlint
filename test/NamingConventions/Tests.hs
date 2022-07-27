module NamingConventions.Tests where

import CheckResults
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Syntax
import NamingConventions
  ( checkLocalName,
    checkMemberName,
    checkMethodName,
    checkPackageName,
    checkParameterName,
    checkStaticVariableName,
    checkTypeName,
  )
import RDF
import RessourceManager
import Test.HUnit

testAll :: IO ()
testAll = do
  testPackageNameTLDOne <- testPackageNameTLDOneIO
  testPackageNameTLDTwo <- testPackageNameTLDTwoIO
  testMethodNameOne <- testMethodNameOneIO
  testMethodNameTwo <- testMethodNameTwoIO
  testMethodNameThree <- testMethodNameThreeIO
  testParameterNameOne <- testParameterNameOneIO
  testParameterNameTwo <- testParameterNameTwoIO
  testParameterNameThree <- testParameterNameThreeIO
  testStaticVarNameOne <- testStaticVarNameOneIO
  testStaticVarNameTwo <- testStaticVarNameTwoIO
  testStaticVarNameThree <- testStaticVarNameThreeIO
  testLocalFinalVarOne <- testLocalFinalVarOneIO
  testLocalFinalVarTwo <- testLocalFinalVarTwoIO
  testLocalFinalVarThree <- testLocalFinalVarThreeIO
  testLocalVarNameOne <- testLocalVarNameOneIO
  testLocalVarNameTwo <- testLocalVarNameTwoIO
  testLocalVarNameThree <- testLocalVarNameThreeIO
  testMemberNameOne <- testMemberNameOneIO
  testMemberNameTwo <- testMemberNameTwoIO
  testMemberNameThree <- testMemberNameThreeIO
  testTypeNameOne <- testTypeNameOneIO
  testTypeNameTwo <- testTypeNameTwoIO
  testTypeNameThree <- testTypeNameThreeIO
  testTypeNameFour <- testTypeNameFourIO
  testTypeNameFive <- testTypeNameFiveIO
  testTypeNameSix <- testTypeNameSixIO
  testTypeNameSeven <- testTypeNameSevenIO
  testTypeNameEight <- testTypeNameEightIO
  testTypeNameNine <- testTypeNameNineIO
  runTestTT
    ( "NamingConventions"
        ~: [ testPackageNameTLDOne,
             testPackageNameTLDTwo,
             testMethodNameOne,
             testMethodNameTwo,
             testMethodNameThree,
             testParameterNameOne,
             testParameterNameTwo,
             testParameterNameThree,
             testStaticVarNameOne,
             testStaticVarNameTwo,
             testStaticVarNameThree,
             testLocalFinalVarOne,
             testLocalFinalVarTwo,
             testLocalFinalVarThree,
             testLocalVarNameOne,
             testLocalVarNameTwo,
             testLocalVarNameThree,
             testMemberNameOne,
             testMemberNameTwo,
             testMemberNameThree,
             testTypeNameOne,
             testTypeNameTwo,
             testTypeNameThree,
             testTypeNameFour,
             testTypeNameFive,
             testTypeNameSix,
             testTypeNameSeven,
             testTypeNameEight,
             testTypeNameNine
           ]
    )
  return ()

{- Package Name -}
testPackageNameTLDOneIO :: IO Test
testPackageNameTLDOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TLDcapitalLetter.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkPackageName cUnit path
                  checkMessage diagResults "PackageName element Dde does not match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("PackageName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testPackageNameTLDTwoIO :: IO Test
testPackageNameTLDTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TLDcapitalLetterSecond.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkPackageName cUnit path
                  checkMessage diagResults "PackageName element dDe does not match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("PackageName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Method Name -}

testMethodNameOneIO :: IO Test
testMethodNameOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/MethodNameCapitalStart.java"
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMethodName cUnit path
                  checkMessage diagResults "Methodname Test does not match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("MethodName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testMethodNameTwoIO :: IO Test
testMethodNameTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/MethodNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMethodName cUnit path
                  checkMessage diagResults "Methodname _Test does not match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("MethodName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testMethodNameThreeIO :: IO Test
testMethodNameThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/MethodNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMethodName cUnit path
                  checkMessage diagResults "Methodname te_st does not match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("MethodName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Parameter Name -}

testParameterNameOneIO :: IO Test
testParameterNameOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/ParameterNameCapitalStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkParameterName cUnit path
                  checkMessage diagResults "Method test: parameter TestVar doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("ParameterName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testParameterNameTwoIO :: IO Test
testParameterNameTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/ParameterNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkParameterName cUnit path
                  checkMessage diagResults "Method test: parameter _testVar doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("ParameterName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testParameterNameThreeIO :: IO Test
testParameterNameThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/ParameterNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkParameterName cUnit path
                  checkMessage diagResults "Method test: parameter test_Var doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("ParameterName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Static Variable Name -}

testStaticVarNameOneIO :: IO Test
testStaticVarNameOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/StaticVarNameCapitalStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkStaticVariableName cUnit path
                  checkMessage diagResults "Static variable Testvar doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("StaticVariableName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testStaticVarNameTwoIO :: IO Test
testStaticVarNameTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/StaticVarNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkStaticVariableName cUnit path
                  checkMessage diagResults "Static variable _testvar doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("StaticVariableName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testStaticVarNameThreeIO :: IO Test
testStaticVarNameThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/StaticVarNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkStaticVariableName cUnit path
                  checkMessage diagResults "Static variable test_var doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("StaticVariableName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Local Final Variable -}

testLocalFinalVarOneIO :: IO Test
testLocalFinalVarOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/LocalFinalVarNameCapitalStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkLocalName cUnit path
                  checkMessage diagResults "Method test: Local final variable Testvar doesn't match the specifications" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("LocalFinalVariable" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testLocalFinalVarTwoIO :: IO Test
testLocalFinalVarTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/LocalFinalVarNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkLocalName cUnit path
                  checkMessage diagResults "Method test: Local final variable _testvar doesn't match the specifications" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("LocalFinalVariable" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testLocalFinalVarThreeIO :: IO Test
testLocalFinalVarThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/LocalFinalVarNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkLocalName cUnit path
                  checkMessage diagResults "Method test: Local final variable test_var doesn't match the specifications" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("LocalFinalVariable" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Local Variable Name -}

testLocalVarNameOneIO :: IO Test
testLocalVarNameOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/LocalVarNameCapitalStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkLocalName cUnit path
                  checkMessage diagResults "Method test: Local non-final variable Testvar doesn't match the specifications" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("LocalVariableName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testLocalVarNameTwoIO :: IO Test
testLocalVarNameTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/LocalVarNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkLocalName cUnit path
                  checkMessage diagResults "Method test: Local non-final variable _testvar doesn't match the specifications" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("LocalVariableName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testLocalVarNameThreeIO :: IO Test
testLocalVarNameThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/LocalVarNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkLocalName cUnit path
                  checkMessage diagResults "Method test: Local non-final variable test_var doesn't match the specifications" path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("LocalVariableName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Member Name -}

testMemberNameOneIO :: IO Test
testMemberNameOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/MemberNameCapitalStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMemberName cUnit path
                  checkMessage diagResults "Instance variable Testvar doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("MemberName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testMemberNameTwoIO :: IO Test
testMemberNameTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/MemberNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMemberName cUnit path
                  checkMessage diagResults "Instance variable _testvar doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("MemberName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testMemberNameThreeIO :: IO Test
testMemberNameThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/MemberNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkMemberName cUnit path
                  checkMessage diagResults "Instance variable test_var doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("MemberName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

{- Type Name -}

testTypeNameOneIO :: IO Test
testTypeNameOneIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameLowercaseStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name testClass doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameTwoIO :: IO Test
testTypeNameTwoIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name _TestClass doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameThreeIO :: IO Test
testTypeNameThreeIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name Test_Class doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameFourIO :: IO Test
testTypeNameFourIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameInterfaceLowercaseStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name testClass doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameFiveIO :: IO Test
testTypeNameFiveIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameInterfaceUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name _TestClass doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameSixIO :: IO Test
testTypeNameSixIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameInterfaceUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name Test_Class doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameSevenIO :: IO Test
testTypeNameSevenIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameEnumLowercaseStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name testClass doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameEightIO :: IO Test
testTypeNameEightIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameEnumUnderscoreStart.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name _TestClass doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.

testTypeNameNineIO :: IO Test
testTypeNameNineIO =
  do
    assertionList <-
      withCUnit -- provides compilation unit for each test - file - fragment. Manages test - setup and teardown
        "/test/NamingConventions/TypeNameEnumUnderscoreLater.java" -- path to testfile
        ( return
            . map -- lamdafunction will be used to create a testcase for each testfile.
              ( \inputCode -> do
                  (cUnit, path) <- inputCode
                  let diagResults = checkTypeName cUnit path
                  checkMessage diagResults "Type name Test_Class doesn't match the specifications." path
                  checkPath diagResults path -- assertion will only be executed if first does succed. this is importend, cause ther will be no path returned when there is no result
              )
        )

    return ("TypeName" ~: test (map TestCase assertionList)) -- create testcase from each Assertion. Merges all testCases into a single test.
