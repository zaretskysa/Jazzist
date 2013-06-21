module Evaluating.InternalValue
(
    module Evaluating.Reference,

    InternalValue(..)
) where

import Evaluating.Reference

data InternalValue = 
    RefValue Reference
    | DoubleValue Double
    deriving (Show)



