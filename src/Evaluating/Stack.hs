module Evaluating.Stack
(
    Stack,

    empty,
    push,
    pop,
    null,
    top

) where

import Prelude(Show, Bool, error)
import qualified Data.List as List

data Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)
pop (Stack []) = error "Trying to perform pop operation on empty stack"

null :: Stack a -> Bool
null (Stack xs) = List.null xs

top :: Stack a -> a
top (Stack (x:_xs)) = x
top (Stack []) = error "Trying to perform pop operation on empty stack"
