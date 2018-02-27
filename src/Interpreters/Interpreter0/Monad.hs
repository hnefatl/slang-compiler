{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreters.Interpreter0.Monad
(
    Interpreter0,
    getEnvironment,
    getValue,
    getValueE,
    setValue,
    removeValue,
    local,
    runtimeError,
    runInterpreter0Monad,

    liftIO -- Export liftIO from the Control.Monad.IO.Class module, as it's useful for doing IO actions in this monad
) where

import Interpreters.Error

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

type Environment k v = M.Map k v

newtype Interpreter0 k v a = Interpreter0
    {
        run :: StateT (Environment k v) (ExceptT Error IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Environment k v), MonadError Error)

getEnvironment :: Interpreter0 k v (Environment k v)
getEnvironment = get

getValue :: Ord k => k -> Interpreter0 k v (Maybe v)
getValue k = gets (M.lookup k)

getValueE :: Ord k => k -> Error -> Interpreter0 k v v
getValueE k e = do
                    val <- getValue k
                    case val of
                        Just v  -> return v
                        Nothing -> throwError e

setValue :: Ord k => k -> v -> Interpreter0 k v ()
setValue k v = modify (M.insert k v)

removeValue :: Ord k => k -> Interpreter0 k v ()
removeValue k = modify (M.delete k)

local :: Ord k => k -> v -> Interpreter0 k v a -> Interpreter0 k v a
local k v a = do
                existingValue <- getValue k
                setValue k v
                res <- a
                case existingValue of
                    Just oldV -> setValue k oldV
                    Nothing   -> removeValue k
                return res

runtimeError :: Error -> Interpreter0 k v a
runtimeError = throwError

runInterpreter0Monad :: Interpreter0 k v a -> IO (Either Error a)
runInterpreter0Monad a = runExceptT $ evalStateT (run a) M.empty