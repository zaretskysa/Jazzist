{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.CommentTests
(
    htf_thisModulesTests
) where

import Control.Applicative
import Test.Framework

import Lexing.Comment
import Lexing.Token
import TestUtils

successsful input result = assertEqual
    (Just $ CommentToken result)
    (tokenFromLocated <$> parseWholeTestInput comment input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput comment input)

test_emptySingleLineComment = successsful "//" ""

test_whitespaceInSingleLineComment = successsful "// " " "

test_emptySingleLineCommentWithEndingLineTerminator = failed "//\n"

test_oneWordSingleLineComment = successsful "//hello" "hello"

test_emptyMultiLineComment = successsful "/**/" ""

test_whitespaceMultiLineComment = successsful "/* */" " "

test_oneWordMultiLineComment = successsful "/*hello*/" "hello"

test_lineTerminatorMultiLineComment = successsful "/*\n*/" "\n"

test_twoWordsWithLineTerminatorMultiLineComment = successsful "/*hello\ngalaxy*/" "hello\ngalaxy"

test_oneSlashSingleLineComment = failed "/hello"

test_multiLineCommentWithoutAsterisk = failed "/*/"

test_multiLineCommentWithoutSlash = failed "/**"

test_bunchOfAsterisksMultiLineComment = successsful "/*****/" "***"

test_bunchOfBackSlashesSingleLineComment = successsful "/////" "///"
