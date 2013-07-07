module Evaluating.ReferenceM
(
    getValue
) where

import Evaluating.Eval
import Evaluating.InternalValue
import qualified Evaluating.Object as Obj
import qualified Evaluating.Reference as Ref
import qualified Evaluating.ConversionM as ConvM

getValue :: InternalValue -> Eval InternalValue
getValue (RefValue ref) = 
    case Ref.isPropertyRef ref of
        True -> 
            get (Ref.refBaseValue ref) (Ref.getReferencedName ref)
            where get = primitiveGet
        False = undefined
getValue value = value

primitiveGet :: Ref.RefBaseValue -> String -> Eval InternalValue
primitiveGet base name = do
    obj <- ConvM.toObject (Ref.baseToValue base)
    case Obj.getProperty obj name of
        Nothing -> JsValue $ UndefinedValue
        _ -> undefined


