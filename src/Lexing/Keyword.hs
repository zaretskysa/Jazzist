module Lexing.Keyword
(
	module Lexing.LocatedToken,

	keyword,
	isKeyword
) where

import Text.ParserCombinators.Parsec

import Lexing.LocatedToken

keyword :: Parser LocatedToken
keyword = do 
	pos <- getPosition
	value <- keyword'
	return $ LocatedToken (KeywordToken value) pos

keyword' :: Parser Keyword
keyword' = do
	value <- many1 letter
	case value of
		"break" -> return BreakKeyword
		"case" -> return CaseKeyword
		"catch" -> return CatchKeyword
		"continue" -> return ContinueKeyword
		"debugger" -> return DebuggerKeyword
		"default" -> return DefaultKeyword
		"delete" -> return DeleteKeyword
		"do" -> return DoKeyword
		"else" -> return ElseKeyword
		"finally" -> return FinallyKeyword
		"for" -> return ForKeyword
		"function" -> return FunctionKeyword
		"if" -> return IfKeyword
		"in" -> return InKeyword
		"instanceof" -> return InstanceOfKeyword
		"new" -> return NewKeyword
		"return" -> return ReturnKeyword
		"switch" -> return SwitchKeyword
		"this" -> return ThisKeyword
		"throw" -> return ThrowKeyword
		"try" -> return TryKeyword
		"typeof" -> return TypeOfKeyword
		"var" -> return VarKeyword
		"void" -> return VoidKeyword
		"while" -> return WhileKeyword
		"with" -> return WithKeyword
		_ -> fail "not a keyword"

keywordIds :: [String]
keywordIds = 
	["break", "case", "catch", "continue", "debugger", "default", "delete",
	"do", "else", "finally", "for", "function", "if", "in", "instanceof", "new",
	"return", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with"]

isKeyword :: String -> Bool
isKeyword s = s `elem` keywordIds