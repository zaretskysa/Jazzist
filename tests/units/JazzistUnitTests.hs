import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import LexingTests.BooleanLiteralTests
import LexingTests.PunctuatorTests

allTests :: Test
allTests = TestList 
	[ booleanLiteralTests
	, punctuatorTests ]

main :: IO ()
main = do 
	counts <- runTestTT allTests
	case counts of 
		Counts _ _ 0 0 -> exitSuccess
		_ -> exitFailure