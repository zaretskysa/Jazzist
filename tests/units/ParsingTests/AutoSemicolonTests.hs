{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.AutoSemicolonTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> Program -> Assertion
successful input expected = 
    case programFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but parsing error occurred"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case programFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)

test_autoSemicolon1 = successful "1 \n" (makeProgram "1;")

test_autoSemicolon2 = successful "1 \n 2;" (makeProgram "1; 2;")

test_autoSemicolon3 = failed "{ 1 2 } 3"

test_autoSemicolon4 = successful "{ 1 \n 2 } 3" (makeProgram "{ 1; 2; } 3;")

test_autoSemicolon5 = failed "for (a; b) "


test_autoSemicolon6 = successful "return \n a;" (makeProgram "return; a;")

test_autoSemicolon7 = successful "continue \n a;" (makeProgram "continue; a;")

test_autoSemicolon8 = successful "break \n a;" (makeProgram "break; a;")


test_autoSemicolon9 = failed "throw \n a;"

test_autoSemicolon10 = successful "return \n" (makeProgram "return;")

test_autoSemicolon11 = successful "continue \n" (makeProgram "continue;")

test_autoSemicolon12 = successful "break \n" (makeProgram "break;")


test_autoSemicolon13 = failed "throw"

test_autoSemicolon14 = successful "return" (makeProgram "return;")

test_autoSemicolon15 = successful "continue" (makeProgram "continue;")

test_autoSemicolon16 = successful "break" (makeProgram "break;")


test_autoSemicolon17 = successful "a = b \n ++c" (makeProgram "a = b; ++c")

test_autoSemicolon18 = failed "if (a > b) \n else c = d"

test_autoSemicolon19 = successful 
    "a = b + c \n (d + e).print()" 
    (makeProgram "a = b + c(d + e).print();")


test_autoSemicolon20 = successful "1 * {}" (makeProgram "1 * {};")

test_autoSemicolon21 = successful "{a:1 \n } 3" (makeProgram "{a:1;} 3;")

test_autoSemicolon22 = successful "{a:1 \n} \n 3" (makeProgram "{a:1;} 3;")

test_autoSemicolon23 = successful 
    "{ \n a: \n 1 \n } \n 3" 
    (makeProgram "{a:1 ;} 3;")

test_autoSemicolon24 = failed "{} * 1" 

test_autoSemicolon25 = successful 
    "({}) * 1" 
    (makeProgram "({}) * 1;")

test_autoSemicolon26 = failed "({};)*1"

test_autoSemicolon27 = successful 
    "( \n {} \n ) * 1"
    (makeProgram "({}) * 1;")

test_autoSemicolon28 = failed "{} \n * 1"

test_autoSemicolon29 = successful 
    "{1} 2"
    (makeProgram "{1;} 2;")

test_autoSemicolon30 = failed "{1 2} 3"

test_autoSemicolon31 = successful 
    "{1 \n 2} 3"
    (makeProgram "{1; 2;} 3;")

test_autoSemicolon32 = failed "if (false) x = 1 else x = -1"

test_autoSemicolon34 = failed "x \n ++;"

test_autoSemicolon35 = successful 
    "x \n ++y"
    (makeProgram "x; ++y;")

test_autoSemicolon36 = failed "x \n --;"

test_autoSemicolon37 = successful 
    "x \n --y"
    (makeProgram "x; --y;")

test_autoSemicolon38 = successful
    "var x = 1\n"
    (makeProgram "var x = 1;")

test_autoSemicolon39 = successful
    "var x = 1"
    (makeProgram "var x = 1;")

test_autoSemicolon40 = successful
    "var x = 1 + f(2 + 3)"
    (makeProgram "var x = 1 + f(2 + 3);")

test_autoSemicolon41 = successful
    "debugger"
    (makeProgram "debugger;")

test_autoSemicolon42 = successful
    "debugger \n"
    (makeProgram "debugger;")

test_autoSemicolon43 = successful
    "x \n ++ \n y"
    (makeProgram "x; ++y;")

test_autoSemicolon44 = successful 
    "var z = \n x \n ++ \n ++ \n y"
    (makeProgram "var z = x; ++ ++ y;")

test_autoSemicolon45 = successful 
    "var z= \n x\n + \n ++ \n y"
    (makeProgram "var z = x + ++y;")

test_autoSemicolon46 = successful 
    "z= \n x \n + ++ \n y"
    (makeProgram "z= x + ++y;")

test_autoSemicolon47 = successful 
    "z=\nx\n+    ++\ny\n"
    (makeProgram "z=x +    ++y;")

test_autoSemicolon48 = successful 
    "var z=\nx\n+\n+\n+\ny"
    (makeProgram "var z=x + + + y;")

test_autoSemicolon49 = successful 
    "z=\nx + + + y"
    (makeProgram "z=x + + + y;")

test_autoSemicolon50 = successful 
    "z=\nx\n+    +\n+    +\ny"
    (makeProgram "z=x+    + +    +y;")

test_autoSemicolon51 = successful 
    "for(;;\n) {\n  break;\n}"
    (makeProgram "for(;;) {  break;}")

test_autoSemicolon52 = successful 
    "for(false\n    ;;false\n) {\n  break;\n}"
    (makeProgram "for (false;;false) {  break; }")

test_autoSemicolon53 = successful 
    "for(false\n;\n;\n) {\nbreak;\n}"
    (makeProgram "for ( false ; ; ) { break; }")

test_autoSemicolon54 = successful
    "for(false\n;false\n;\n) {\nbreak;\n}"
    (makeProgram "for ( false; false; ) { break; }")

test_autoSemicolon55 = successful
    "for(false\n;false\n;false\n) {\nbreak;\n}"
    (makeProgram "for ( false; false; false) { break; }")

test_autoSemicolon56 = successful
    "for(;\n;\n) {\nbreak;\n}"
    (makeProgram "for(;;) {break;}")

test_autoSemicolon57 = successful
    "for(\n;;\n) {\nbreak;\n}"
    (makeProgram "for (;;) {break;}")

test_autoSemicolon58 = successful
    "for(\n;\n;\n) {\nbreak;\n}"
    (makeProgram "for(;;) {break;}")

test_autoSemicolon59 = successful
    "for(\n;\n;\n) {\nbreak;\n}"
    (makeProgram "for(;;) {break;}")

test_autoSemicolon60 = successful
    "for(false;false;false\n) {\nbreak;\n}"
    (makeProgram "for(false;false;false) {break;}")

test_autoSemicolon61 = successful
    "for(false;false\n;\n) {\nbreak;\n}"
    (makeProgram "for(false;false;) {break;}")

test_autoSemicolon62 = successful
    "for(false;false\n;false\n) {\nbreak;\n}"
    (makeProgram "for(false;false;false) {break;}")

test_autoSemicolon63 = successful
    "for(false\n;;\n) {\nbreak;\n}"
    (makeProgram "for(false;;) {break;}")

test_autoSemicolon64 = failed
    "for(;\n) {\nbreak;\n}"

test_autoSemicolon65 = failed
    "for(\nfalse\n;) {\nbreak;\n}"

test_autoSemicolon66 = failed
    "for(\n;\n) {\nbreak;\n}"

test_autoSemicolon67 = failed
    "for(\n;) {\nbreak;\n}"

test_autoSemicolon68 = failed
    "for(\n;) {\nbreak;\n}"

test_autoSemicolon69 = failed
    "for(\n;) {\nbreak;\n}"

test_autoSemicolon70 = failed
    "for(false;false\n) {\nbreak;\n}"

test_autoSemicolon71 = failed
    "for(false;\nfalse\n) {\nbreak;\n}"

test_autoSemicolon72 = failed
    "for(false\n;\n) {\nbreak;\n}"

test_autoSemicolon73 = failed
    "for(false\n;false\n) {\nbreak;\n}"

test_autoSemicolon74 = failed
    "for(\n;false) {\nbreak;\n}"

test_autoSemicolon75 = failed
    "for(\n) {\nbreak;\n}"

test_autoSemicolon76 = failed
    "for(\n\n) {\nbreak;\n}"

test_autoSemicolon77 = failed
    "for(\n\n\n) {\nbreak;\n}"

test_autoSemicolon78 = failed
    "for(\nfalse\n) {\nbreak;\n}"

test_autoSemicolon79 = failed
    "for(false\nfalse\n) {\nbreak;\n}"

test_autoSemicolon80 = failed
    "for(\nfalse\nfalse\n) {\nbreak;\n}"

test_autoSemicolon81 = failed
    "for(\nfalse\nfalse\nfalse\n) {\nbreak;\n}"

test_autoSemicolon82 = failed
    "for(false;false;false;) {\nbreak;\n}"

test_autoSemicolon83 = failed
    "for(false;false;;false) {\nbreak;\n}"

test_autoSemicolon84 = successful
    "var x\n= 1"
    (makeProgram "var x = 1;")

test_autoSemicolon85 = successful
    "var x = \n1"
    (makeProgram "var x = 1;")

test_autoSemicolon86 = successful
    "var x\nx = 1"
    (makeProgram "var x; x = 1;")

test_autoSemicolon87 = successful
    "var\nx"
    (makeProgram "var x;")

test_autoSemicolon88 = successful
    "var\nx \n= \n1"
    (makeProgram "var x = 1;")

test_autoSemicolon89 = successful
    "var x, \ny"
    (makeProgram "var x, y;")

test_autoSemicolon90 = successful
    "var x \ny"
    (makeProgram "var x; y;")

test_autoSemicolon91 = successful
    "var x\n,y"
    (makeProgram "var x,y;")

test_autoSemicolon92 = successful
    "var \nx \n,y = 1"
    (makeProgram "var x, y = 1;")

test_autoSemicolon93 = successful
    ";"
    (makeProgram ";")

test_autoSemicolon94 = successful
    ";\n;\n;\n;"
    (makeProgram ";;;;")

test_autoSemicolon95 = successful
    ";;;;"
    (makeProgram ";;;;")

test_autoSemicolon96 = successful
    ";1;\n;1\n;1;\n;1\n"
    (makeProgram ";1;;1;1;;1;")

test_autoSemicolon97 = successful
    ";;1;;1;;1"
    (makeProgram ";;1;;1;;1;")

test_autoSemicolon98 = successful
    "do {  \n} while (false)\n"
    (makeProgram "do {} while (false);")

test_autoSemicolon99 = successful
    "do ; while (false)\ntrue\n"
    (makeProgram "do ; while (false); true;")

test_autoSemicolon100 = successful
    "do {\n;\n} while ((false)\n)\n"
    (makeProgram "do {;} while ((false))")

test_autoSemicolon101 = failed
    "do\nwhile (false) "

test_autoSemicolon102 = failed
    "do\n\nwhile (false)\n\n"

test_autoSemicolon103 = failed
    "do {}; \nwhile (false) \n"

test_autoSemicolon104 = successful
    "do {} \nwhile (false)\n"
    (makeProgram "do {} while (false);")

test_autoSemicolon105 = successful
    "var x"
    (makeProgram "var x;")

test_autoSemicolon106 = successful
    "var x\n"
    (makeProgram "var x;")

test_autoSemicolon107 = successful
    "var x;\n"
    (makeProgram "var x;")

test_autoSemicolon108 = successful
    "do {} while(true)"
    (makeProgram "do {} while(true)")
