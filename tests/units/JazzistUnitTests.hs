import Test.HUnit

testEmpty = TestCase $ assertEqual "1+2 equals to 3" (1+2) 3

main = runTestTT testEmpty