module NamingConventions.Tests where

import CheckResults
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
  runTestTT
    ( TestList
        [ testPackageNameTLDOne,
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
testPackageNameTLDOne :: IO Test
testPackageNameTLDOne =
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

testPackageNameTLDTwo :: IO Test
testPackageNameTLDTwo =
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

testMethodNameOne :: IO Test
testMethodNameOne =
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

testMethodNameTwo :: IO Test
testMethodNameTwo =
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

testMethodNameThree :: IO Test
testMethodNameThree =
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

testParameterNameOne :: IO Test
testParameterNameOne =
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

testParameterNameTwo :: IO Test
testParameterNameTwo =
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

testParameterNameThree :: IO Test
testParameterNameThree =
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

testStaticVarNameOne :: IO Test
testStaticVarNameOne =
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

testStaticVarNameTwo :: IO Test
testStaticVarNameTwo =
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

testStaticVarNameThree :: IO Test
testStaticVarNameThree =
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

testLocalFinalVarOne :: IO Test
testLocalFinalVarOne =
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

testLocalFinalVarTwo :: IO Test
testLocalFinalVarTwo =
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

testLocalFinalVarThree :: IO Test
testLocalFinalVarThree =
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

testLocalVarNameOne :: IO Test
testLocalVarNameOne =
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

testLocalVarNameTwo :: IO Test
testLocalVarNameTwo =
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

testLocalVarNameThree :: IO Test
testLocalVarNameThree =
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

testMemberNameOne :: IO Test
testMemberNameOne =
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

testMemberNameTwo :: IO Test
testMemberNameTwo =
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

testMemberNameThree :: IO Test
testMemberNameThree =
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

testTypeNameOne :: IO Test
testTypeNameOne =
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

testTypeNameTwo :: IO Test
testTypeNameTwo =
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

testTypeNameThree :: IO Test
testTypeNameThree =
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

testTypeNameFour :: IO Test
testTypeNameFour =
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

testTypeNameFive :: IO Test
testTypeNameFive =
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

testTypeNameSix :: IO Test
testTypeNameSix =
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

testTypeNameSeven :: IO Test
testTypeNameSeven =
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

testTypeNameEight :: IO Test
testTypeNameEight =
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

testTypeNameNine :: IO Test
testTypeNameNine =
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
