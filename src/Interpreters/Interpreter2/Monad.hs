{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreters.Interpreter2.Monad
(
    Interpreter2,
    getEnvironment,
    pushValue,
    pushValues,
    popValue,
    popValues,
    runtimeError,
    runInterpreter2Monad
) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

import Interpreters.Error
import Interpreters.Interpreter2.Values

newtype Interpreter2 a = Interpreter2
    {
        run :: StateT (ValueStack, Environment) (ExceptT Error IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (ValueStack, Environment), MonadError Error)

modifyValueStack :: (ValueStack -> ValueStack) -> Interpreter2 ()
modifyValueStack f = modify (\(vs, env) -> (f vs, env))

getEnvironment :: Interpreter2 Environment
getEnvironment = gets snd

getValueStack :: Interpreter2 ValueStack
getValueStack = gets fst

pushValue :: Value -> Interpreter2 ()
pushValue v = modifyValueStack (v:)

pushValues :: [Value] -> Interpreter2 ()
pushValues = mapM_ pushValue

popValue :: Interpreter2 Value
popValue = do
            v:_ <- getValueStack
            modifyValueStack tail
            return v

popValues :: Int -> Interpreter2 [Value]
popValues n = liftM (take n) getValueStack

runtimeError :: Error -> Interpreter2 ()
runtimeError = throwError

runInterpreter2Monad :: Interpreter2 a -> IO (Either Error a)
runInterpreter2Monad a = runExceptT $ evalStateT (run a) ([], M.empty)