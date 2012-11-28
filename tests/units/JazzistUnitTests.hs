import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import LexingTests.BooleanLiteralTests
import LexingTests.PunctuatorTests
import LexingTests.KeywordTests

allTests :: Test
allTests = TestList 
	[ booleanLiteralTests
	, punctuatorTests
	, keywordTests ]

main :: IO ()
main = do 
	counts <- runTestTT allTests
	case counts of 
		Counts _ _ 0 0 -> exitSuccess
		_ -> exitFailure