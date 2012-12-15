import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import LexingTests.BooleanLiteralTests
import LexingTests.PunctuatorTests
import LexingTests.KeywordTests
import LexingTests.NumericLiteralTests
import LexingTests.NullLiteralTests
import LexingTests.CommentTests
import LexingTests.StringLiteralTests

allTests :: Test
allTests = TestList 
	[ booleanLiteralTests
	, punctuatorTests
	, keywordTests
	, numericLiteralTests
    , nullLiteralTests
    , commentTests
    , stringLiteralTests
    ]

main :: IO ()
main = do 
	counts <- runTestTT allTests
	case counts of 
		Counts _ _ 0 0 -> exitSuccess
		_ -> exitFailure