module Evaluating.ConversionM
(
    Value(..),

    toObject
) where

import Evaluating.Value
import Evaluating.Eval
import qualified Evaluating.Conversion as Conv

toObject :: Value -> Eval Object
toObject val = do
    case Conv.toObject of
        Just obj -> return obj
        Nothing = error "js exception"
