module Parser.Types
(
    Type(..)
) where

data Type   = Int
            | Bool
            | Unit
            | Ref Type
            | Fn Type Type
            | Union Type Type
            | Product Type Type
            deriving (Eq, Show)