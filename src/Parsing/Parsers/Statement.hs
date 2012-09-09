module Parsing.Parsers.Statement where

import Parsing.Data.Statement
import Parsing.TokenParser

statement :: TokenParser Statement
statement = blockStatement

blockStatement :: TokenParser Statement
blockStatement = do
    acceptToken $ PunctuatorToken LeftCurlyBracketPunctuator
    stmts <- many statement
    acceptToken $ PunctuatorToken RightCurlyBracketPunctuator
    return $ BlockStatement stmts

