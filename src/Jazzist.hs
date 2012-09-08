-- Jazzist entry point

import System.Environment

import Lexing.Lexer

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input provided"
        else putStrLn $ readTokens $ head args
