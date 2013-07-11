module Common.BoolUtils
(
    xor,
) where

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

