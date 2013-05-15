{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.KeywordTests
(
	htf_thisModulesTests
) where

import Control.Applicative ((<$>))
import Test.Framework

import Lexing.Keyword
import Lexing.Token
import TestUtils

successful input result = assertEqual
    (Just $ KeywordToken result)
    (tokenFromLocated <$> parseWholeTestInput keyword input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput keyword input)

test_breakKeyword = successful "break" BreakKeyword

test_caseKeyword = successful "case" CaseKeyword

test_catchKeyword = successful "catch" CatchKeyword

test_continueKeyword = successful "continue" ContinueKeyword

test_debuggerKeyword = successful "debugger" DebuggerKeyword

test_defaultKeyword = successful "default" DefaultKeyword

test_deleteKeyword = successful "delete" DeleteKeyword

test_doKeyword = successful "do" DoKeyword

test_elseKeyword = successful "else" ElseKeyword

test_finallyKeyword = successful "finally" FinallyKeyword

test_forKeyword = successful "for" ForKeyword

test_functionKeyword = successful "function" FunctionKeyword

test_ifKeyword = successful "if" IfKeyword

test_inKeyword = successful "in" InKeyword

test_instanceOfKeyword = successful "instanceof" InstanceOfKeyword

test_newKeyword = successful "new" NewKeyword

test_returnKeyword = successful "return" ReturnKeyword

test_switchKeyword = successful "switch" SwitchKeyword

test_thisKeyword = successful "this" ThisKeyword

test_throwKeyword = successful "throw" ThrowKeyword

test_tryKeyword = successful "try" TryKeyword

test_typeOfKeyword = successful "typeof" TypeOfKeyword

test_varKeyword = successful "var" VarKeyword

test_voidKeyword = successful "void" VoidKeyword

test_whileKeyword = successful "while" WhileKeyword

test_withKeyword = successful "with" WithKeyword
