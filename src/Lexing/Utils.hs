module Lexing.Utils
(
    intFromHex
) where

import Numeric

intFromHex :: String -> Int
intFromHex = fst . head . readHex
