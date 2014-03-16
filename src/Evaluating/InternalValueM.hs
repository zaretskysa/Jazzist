module Evaluating.InternalValueM
(
    getValueM,
    putValueM
) where


import Evaluating.Eval
import Evaluating.InternalValue

getValueM :: InternalValue -> Eval InternalValue
getValueM val = undefined

putValueM :: InternalValue -> Value -> Eval InternalValue
putValueM lref rval = undefined
