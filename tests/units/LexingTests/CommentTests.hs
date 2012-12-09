module LexingTests.CommentTests
(
    commentTests
) where

import Test.HUnit
import Lexing.Comment
import Lexing.Tokens
import TestUtils

commentTests :: Test
commentTests = TestLabel "Comment tests" $ TestList 
    [ emptySingleLineComment
    , whitespaceInSingleLineComment
    , emptySingleLineCommentWithEndingLineTerminator
    , emptyMultiLineComment
    , whitespaceMultiLineComment
    , oneWordMultiLineComment
    , lineTerminatorMultiLineComment
    , twoWordsWithLineTerminatorMultiLineComment
    , oneSlashSingleLineComment
    , multiLineCommentWithoutAsterisk
    , multiLineCommentWithoutSlash
    , bunchOfAsterisksMultiLineComment
    , bunchOfBackSlashesSingleLineComment
    ]

successfulCommentParsing :: String -> String -> Test
successfulCommentParsing input result = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ CommentToken result)
    (parseWholeTestInput comment input)

failedCommentParsing :: String -> Test
failedCommentParsing input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput comment input)

emptySingleLineComment :: Test
emptySingleLineComment = successfulCommentParsing "//" ""

whitespaceInSingleLineComment :: Test
whitespaceInSingleLineComment = successfulCommentParsing "// " " "

emptySingleLineCommentWithEndingLineTerminator :: Test
emptySingleLineCommentWithEndingLineTerminator = failedCommentParsing "//\n"

oneWordSingleLineComment :: Test
oneWordSingleLineComment = successfulCommentParsing "//hello" "hello"

emptyMultiLineComment :: Test
emptyMultiLineComment = successfulCommentParsing "/**/" ""

whitespaceMultiLineComment :: Test
whitespaceMultiLineComment = successfulCommentParsing "/* */" " "

oneWordMultiLineComment :: Test
oneWordMultiLineComment = successfulCommentParsing "/*hello*/" "hello"

lineTerminatorMultiLineComment :: Test
lineTerminatorMultiLineComment = successfulCommentParsing "/*\n*/" "\n"

twoWordsWithLineTerminatorMultiLineComment :: Test
twoWordsWithLineTerminatorMultiLineComment = successfulCommentParsing "/*hello\ngalaxy*/" "hello\ngalaxy"

oneSlashSingleLineComment :: Test
oneSlashSingleLineComment = failedCommentParsing "/hello"

multiLineCommentWithoutAsterisk :: Test
multiLineCommentWithoutAsterisk = failedCommentParsing "/*/"

multiLineCommentWithoutSlash :: Test
multiLineCommentWithoutSlash = failedCommentParsing "/**"

bunchOfAsterisksMultiLineComment :: Test
bunchOfAsterisksMultiLineComment = successfulCommentParsing "/*****/" "***"

bunchOfBackSlashesSingleLineComment :: Test
bunchOfBackSlashesSingleLineComment = successfulCommentParsing "/////" "///"
