module Utils where

import Numeric

intFromHex :: String -> Int
intFromHex = fst . head . readHex