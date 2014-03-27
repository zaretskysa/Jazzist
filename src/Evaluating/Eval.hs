module Evaluating.Eval
(
    module Control.Monad.Identity,
    module Control.Monad.State,

    Eval,

    runEval,

    activeLexEnv
) where

import Control.Monad.Identity
import Control.Monad.State
import qualified Evaluating.Environment as Env
import Evaluating.ExecutionContext

type Eval a = StateT Env.Environment Identity a

runEval :: Eval a -> (a, Env.Environment)
runEval eval = runIdentity (runStateT eval Env.newEnvironment)

activeLexEnv :: Eval (Maybe LexicalEnvironment)
activeLexEnv = undefined
