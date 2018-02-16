module Parser.Types
(
    Type(..),
    isInteger,
    isBoolean,
    isUnit,
    isRef,
    isFn,
    isUnion,
    isProduct
) where

data Type   = Integer
            | Boolean
            | Unit
            | Ref Type
            | Fn Type Type
            | Union Type Type
            | Product Type Type
            deriving (Eq, Show)

isInteger :: Type -> Bool
isInteger Integer = True
isInteger _       = False

isBoolean :: Type -> Bool
isBoolean Boolean = True
isBoolean _       = False

isUnit :: Type -> Bool
isUnit Unit = True
isUnit _       = False

isRef :: Type -> Bool
isRef (Ref _) = True
isRef _       = False

isFn :: Type -> Bool
isFn (Fn _ _) = True
isFn _        = False

isUnion :: Type -> Bool
isUnion (Union _ _) = True
isUnion _           = False

isProduct :: Type -> Bool
isProduct (Product _ _) = True
isProduct _             = False