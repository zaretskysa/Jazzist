import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import LexingTests.BooleanLiteralTests

allTests :: Test
allTests = TestList 
	[ booleanLiteralTests ]

main :: IO ()
main = do 
	counts <- runTestTT allTests
	case counts of 
		Counts _ _ 0 0 -> exitSuccess
		_ -> exitFailure