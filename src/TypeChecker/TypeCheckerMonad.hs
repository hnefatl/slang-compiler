{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker.TypeCheckerMonad
(
    TypeChecker,
    inModifiedEnv,
    fromEnv,
    typeError,
    runTypeChecker
) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map.Lazy as M

import Common
import TypeChecker.Error

type Environment k v = M.Map k v

newtype TypeChecker k v a = TypeChecker
    {
        run :: ReaderT (Environment k v) (Except Error) a
    }
    deriving (Functor, Applicative, Monad, MonadReader (Environment k v), MonadError Error)

inModifiedEnv :: (Environment k v -> Environment k v) -> TypeChecker k v a -> TypeChecker k v a
inModifiedEnv = local

fromEnv :: (Environment k v -> a) -> TypeChecker k v a
fromEnv = asks

typeError :: Error -> TypeChecker k v a
typeError = throwError

runTypeChecker :: TypeChecker k v a -> Either FrontEndError a
runTypeChecker t = case runExcept $ runReaderT (run t) M.empty of
                    Left e  -> Left $ convertError e -- Convert the error from internal to external
                    Right r -> Right r