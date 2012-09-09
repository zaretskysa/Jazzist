-- Jazzist entry point

import System.Environment

import Parsing.Parser

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input provided"
        else putStrLn $ tryToParseString $ head args
