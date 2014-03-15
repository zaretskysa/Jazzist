module Common.Result
(
    Result(..)
) where

data Result a = Success a | Fail deriving (Eq, Show)
