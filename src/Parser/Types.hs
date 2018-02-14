module Parser.Types
(
    Type(..)
) where

data Type   = TInt
            | TBool
            | TUnit
            | TRef Type
            | TFn Type Type
            | TUnion Type Type
            | TProduct Type Type
            deriving (Eq, Show)