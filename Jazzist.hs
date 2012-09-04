-- Jazzist entry point

import System.Environment

import Lexer

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input provided"
        else putStrLn $ readTokens $ head args
