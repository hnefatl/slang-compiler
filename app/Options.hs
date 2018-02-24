{-# LANGUAGE TemplateHaskell #-}

module Options
(
    Options(..),
    parseOptions
) where

import Control.Monad.State
import Control.Monad.Except
import Control.Lens (makeLenses, set)

import Common

data Options = Options
    {
        _file :: Maybe FilePath,
        _interpreter0 :: Bool
    }
makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options { _file = Nothing, _interpreter0 = False }

parseOptions :: [String] -> Either Error Options
parseOptions args = runExcept (execStateT (parseOption args) defaultOptions)

parseOption :: [String] -> StateT Options (Except Error) ()
parseOption ("-I0":args) = modify (set interpreter0 True) >> parseOption args
parseOption [filePath]   = modify (set file (Just filePath))
parseOption (op:_)       = throwError ("Unknown option " ++ op)
parseOption [] = return ()