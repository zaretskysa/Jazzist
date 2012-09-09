module Parsing.Parsers.SourceElement where

import Parsing.TokenParser
import Parsing.Data.Program
import Parsing.Parsers.Statement

sourceElement :: TokenParser SourceElement
sourceElement = statementSourceElement -- <|> functionDeclarationSourceElement

statementSourceElement :: TokenParser SourceElement
statementSourceElement = do
    stmt <- statement
    return $ StatementSourceElement stmt

functionDeclarationSourceElement :: TokenParser SourceElement
functionDeclarationSourceElement = undefined
