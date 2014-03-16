module Evaluating.InternalValue
(
    module Evaluating.Value,
    
    InternalValue(..),

    getValue,
    putValue
) where

import qualified Evaluating.Reference as Ref
import Evaluating.Value

data InternalValue = 
      RefValue Ref.Reference
    | JsValue Value
    | DoubleValue Double
    deriving (Show)

refBaseToValue :: Ref.RefBaseValue -> Value
refBaseToValue Ref.UndefinedRefVal = UndefinedValue
refBaseToValue (Ref.ObjectRefVal obj) = ObjectValue obj
refBaseToValue (Ref.BoolRefVal bool) = BooleanValue bool
refBaseToValue (Ref.StringRefVal str) = StringValue str
refBaseToValue (Ref.NumberRefVal num) = NumberValue num
refBaseToValue (Ref.EnvRecordRefVal _) = undefined

getValue :: InternalValue -> InternalValue
getValue (RefValue ref) = JsValue $ refBaseToValue $ Ref.getRefValue ref
getValue val@(JsValue _) = val
getValue (DoubleValue _) = undefined

putValue :: InternalValue -> InternalValue -> InternalValue
putValue _lref _rval = undefined
