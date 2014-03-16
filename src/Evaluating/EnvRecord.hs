module Evaluating.EnvRecord
(
    EnvRecord(..)
) where

import Evaluating.Value

class EnvRecord record where
    hasBinding :: record -> String -> Bool
    createMutableBinding :: record -> String -> Bool -> record
    setMutableBinding :: record -> String -> Value -> Bool -> record
    getBindingValue :: record -> String -> Bool -> Value
    deleteBinding :: record -> String -> Bool
    implicitThisValue :: record -> Value
