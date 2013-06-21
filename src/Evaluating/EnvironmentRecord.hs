module Evaluating.EnvironmentRecord
(
    EnvironmentRecord(..),

    hasBinding
) where

data EnvironmentRecord = EnvironmentRecord 
    deriving (Show)

hasBinding :: EnvironmentRecord -> String -> Bool
hasBinding envRec name = undefined

