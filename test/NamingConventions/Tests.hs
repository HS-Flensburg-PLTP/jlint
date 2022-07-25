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
          testMemberNameThree
        ]
    )
  return ()

{- Package Name -}

testPackageNameTLDOne =
  "Test TopLevelDomain begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/TLDcapitalLetter.java"
          ( \(path, cUnit) -> do
              let diagResult = checkPackageName cUnit path
              checkPath diagResult path
              checkMessage diagResult "PackageName element Dde does not match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testPackageNameTLDTwo =
  "Test TopLevelDomain contain capital letter after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/TLDcapitalLetterSecond.java"
          ( \(path, cUnit) -> do
              let diagResult = checkPackageName cUnit path
              checkPath diagResult path
              checkMessage diagResult "PackageName element dDe does not match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

{- Method Name -}

testMethodNameOne =
  "Test MethodName begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/MethodNameCapitalStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMethodName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Methodname Test does not match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testMethodNameTwo =
  "Test MethodName begin with underscore" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/MethodNameUnderscoreStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMethodName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Methodname _Test does not match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testMethodNameThree =
  "Test MethodName contain underscore after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/MethodNameUnderscoreLater.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMethodName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Methodname te_st does not match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

{- Parameter Name -}

testParameterNameOne =
  "Test ParameterName begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/ParameterNameCapitalStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkParameterName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: parameter TestVar doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testParameterNameTwo =
  "Test ParameterName begin with underscore" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/ParameterNameUnderscoreStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkParameterName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: parameter _testVar doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testParameterNameThree =
  "Test ParameterName contain underscore after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/ParameterNameUnderscoreLater.java"
          ( \(path, cUnit) -> do
              let diagResult = checkParameterName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: parameter test_Var doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

{- Static Variable Name -}

testStaticVarNameOne =
  "Test StaticVariableName begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/StaticVarNameCapitalStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkStaticVariableName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Static variable Testvar doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testStaticVarNameTwo =
  "Test StaticVariableName begin with underscore" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/StaticVarNameUnderscoreStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkStaticVariableName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Static variable _testvar doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testStaticVarNameThree =
  "Test StaticVariableName contain underscore after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/StaticVarNameUnderscoreLater.java"
          ( \(path, cUnit) -> do
              let diagResult = checkStaticVariableName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Static variable test_var doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

{- Local Final Variable -}

testLocalFinalVarOne =
  "Test LocalFinalVariable begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/LocalFinalVarNameCapitalStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkLocalName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: Local final variable Testvar doesn't match the specifications"
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testLocalFinalVarTwo =
  "Test LocalFinalVariable begin with underscore" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/LocalFinalVarNameUnderscoreStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkLocalName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: Local final variable _testvar doesn't match the specifications"
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testLocalFinalVarThree =
  "Test LocalFinalVariable contain underscore after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/LocalFinalVarNameUnderscoreLater.java"
          ( \(path, cUnit) -> do
              let diagResult = checkLocalName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: Local final variable test_var doesn't match the specifications"
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

{- Local Variable Name -}

testLocalVarNameOne =
  "Test LocalVariableName begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/LocalVarNameCapitalStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkLocalName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: Local non-final variable Testvar doesn't match the specifications"
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testLocalVarNameTwo =
  "Test LocalVariableName begin with underscore" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/LocalVarNameUnderscoreStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkLocalName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: Local non-final variable _testvar doesn't match the specifications"
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testLocalVarNameThree =
  "Test LocalVariableName contain underscore after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/LocalVarNameUnderscoreLater.java"
          ( \(path, cUnit) -> do
              let diagResult = checkLocalName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Method test: Local non-final variable test_var doesn't match the specifications"
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

{- Member Name -}

testMemberNameOne =
  "Test MemberName begin with capital letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/MemberNameCapitalStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMemberName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Instance variable Testvar doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testMemberNameTwo =
  "Test MemberName begin with underscore" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/MemberNameUnderscoreStart.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMemberName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Instance variable _testvar doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]

testMemberNameThree =
  "Test MemberName contain underscore after first letter" -- ~: adds label to Test (Testlist)
    ~: test -- test can be called on List of testables
      [ withCUnit
          "/test/NamingConventions/MemberNameUnderscoreLater.java"
          ( \(path, cUnit) -> do
              let diagResult = checkMemberName cUnit path
              checkPath diagResult path
              checkMessage diagResult "Instance variable test_var doesn't match the specifications."
              -- diagResult @=? [expected] -- asserition, can do multiple in one Test, which would only be exuted if the forme succeded
          )
      ]
