module Parser.Types
(
    Type(..)
) where

data Type   = Integer
            | Boolean
            | Unit
            | Ref Type
            | Fn Type Type
            | Union Type Type
            | Product Type Type
            deriving (Eq, Show)