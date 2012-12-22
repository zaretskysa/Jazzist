module Lexing.Comment where

import Text.ParserCombinators.Parsec

import Lexing.Token
import Lexing.LineTerminator

comment :: Parser Token
comment = do
    value <- try multiLineComment <|> singleLineComment
    return $ CommentToken value

multiLineComment :: Parser String
multiLineComment = do
    string "/*"
    values <- option "" multiLineCommentChars
    string "*/"
    return values

multiLineCommentChars :: Parser String
multiLineCommentChars = do
    multiLineCommentCharsBeginigWithNotAsterisk
    <|> multiLineCommentCharsBeginigWithAsterisk

multiLineCommentCharsBeginigWithAsterisk :: Parser String   
multiLineCommentCharsBeginigWithAsterisk = do
    try $ notFollowedBy $ string "*/"
    begin <- char '*'
    rest <- option "" postAsteriskCommentChars
    return $ begin : rest

postAsteriskCommentChars :: Parser String
postAsteriskCommentChars = do
    postAsteriskCommentCharsAlernative1
    <|> postAsteriskCommentCharsAlernative2
    
postAsteriskCommentCharsAlernative1 :: Parser String
postAsteriskCommentCharsAlernative1 = do
    begin <- multiLineNotForwardSlashOrAsteriskChar
    rest <- option "" multiLineCommentChars
    return $ begin : rest

multiLineNotForwardSlashOrAsteriskChar :: Parser Char
multiLineNotForwardSlashOrAsteriskChar = do
    try $ notFollowedBy $ oneOf "*/"
    anyChar

postAsteriskCommentCharsAlernative2 :: Parser String
postAsteriskCommentCharsAlernative2 = do
    try $ notFollowedBy $ string "*/"
    begin <- char '*'
    rest <- option "" postAsteriskCommentChars
    return $ begin : rest

multiLineCommentCharsBeginigWithNotAsterisk :: Parser String
multiLineCommentCharsBeginigWithNotAsterisk= do
    begin <- multiLineNotAsteriskChar
    rest <- option "" multiLineCommentChars
    return $ begin : rest

multiLineNotAsteriskChar :: Parser Char
multiLineNotAsteriskChar = do
    try $ notFollowedBy $ char '*'
    anyChar

singleLineComment :: Parser String
singleLineComment = string "//" >> option "" singleLineCommentChars

singleLineCommentChars :: Parser String
singleLineCommentChars = many1 singleLineCommentChar

singleLineCommentChar :: Parser Char
singleLineCommentChar = do
    try $ notFollowedBy lineTerminator
    anyChar
