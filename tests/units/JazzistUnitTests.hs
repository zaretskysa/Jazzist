{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} LexingTests.BooleanLiteralTests
import {-@ HTF_TESTS @-} LexingTests.CommentTests
import {-@ HTF_TESTS @-} LexingTests.KeywordTests
import {-@ HTF_TESTS @-} LexingTests.LineTerminatorTests
import {-@ HTF_TESTS @-} LexingTests.NullLiteralTests
import {-@ HTF_TESTS @-} LexingTests.NumericLiteralTests
import {-@ HTF_TESTS @-} LexingTests.PunctuatorTests
import {-@ HTF_TESTS @-} LexingTests.StringLiteralTests
import {-@ HTF_TESTS @-} LexingTests.WhiteSpaceTests
import {-@ HTF_TESTS @-} LexingTests.LexerTests

main :: IO ()
main = htfMain htf_importedTests
