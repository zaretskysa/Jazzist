module LexingTests.KeywordTests
(
	keywordTests
) where

import Test.HUnit
import Lexing.Keyword
import Lexing.Token
import TestUtils

keywordTests :: Test
keywordTests = TestLabel "KeywordTests" $ TestList 
	[ breakKeyword
	, caseKeyword
	, catchKeyword
	, continueKeyword
	, debuggerKeyword
	, defaultKeyword
	, deleteKeyword
	, doKeyword
	, elseKeyword
	, finallyKeyword
	, forKeyword
	, functionKeyword
	, ifKeyword
	, inKeyword
	, instanceOfKeyword
	, newKeyword
	, returnKeyword
	, switchKeyword
	, thisKeyword
	, throwKeyword
	, tryKeyword
	, typeOfKeyword
	, varKeyword
	, voidKeyword
	, whileKeyword
	, withKeyword ]

successful :: String -> Keyword -> Test
successful input result = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ KeywordToken result)
    (parseWholeTestInput keyword input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput keyword input)

breakKeyword :: Test
breakKeyword = successful "break" BreakKeyword

caseKeyword :: Test
caseKeyword = successful "case" CaseKeyword

catchKeyword :: Test
catchKeyword = successful "catch" CatchKeyword

continueKeyword :: Test
continueKeyword = successful "continue" ContinueKeyword

debuggerKeyword :: Test
debuggerKeyword = successful "debugger" DebuggerKeyword

defaultKeyword :: Test
defaultKeyword = successful "default" DefaultKeyword

deleteKeyword :: Test
deleteKeyword = successful "delete" DeleteKeyword

doKeyword :: Test
doKeyword = successful "do" DoKeyword

elseKeyword :: Test
elseKeyword = successful "else" ElseKeyword

finallyKeyword :: Test
finallyKeyword = successful "finally" FinallyKeyword

forKeyword :: Test
forKeyword = successful "for" ForKeyword

functionKeyword :: Test
functionKeyword = successful "function" FunctionKeyword

ifKeyword :: Test
ifKeyword = successful "if" IfKeyword

inKeyword :: Test
inKeyword = successful "in" InKeyword

instanceOfKeyword :: Test
instanceOfKeyword = successful "instanceof" InstanceOfKeyword

newKeyword :: Test
newKeyword = successful "new" NewKeyword

returnKeyword :: Test
returnKeyword = successful "return" ReturnKeyword

switchKeyword :: Test
switchKeyword = successful "switch" SwitchKeyword

thisKeyword :: Test
thisKeyword = successful "this" ThisKeyword

throwKeyword :: Test
throwKeyword = successful "throw" ThrowKeyword

tryKeyword :: Test
tryKeyword = successful "try" TryKeyword

typeOfKeyword :: Test
typeOfKeyword = successful "typeof" TypeOfKeyword

varKeyword :: Test
varKeyword = successful "var" VarKeyword

voidKeyword :: Test
voidKeyword = successful "void" VoidKeyword

whileKeyword :: Test
whileKeyword = successful "while" WhileKeyword

withKeyword :: Test
withKeyword = successful "with" WithKeyword
