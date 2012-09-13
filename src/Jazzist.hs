-- Jazzist entry point

import System.Environment
import Text.Show.Pretty

import Parsing.Parser

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input provided"
        else putStrLn $ ppShow $ parseString $ head args
