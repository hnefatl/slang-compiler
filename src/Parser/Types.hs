module Parser.Types
(
    Type(..),
    isInteger,
    isBoolean,
    isUnit,
    isRef,
    isFun,
    isUnion,
    isProduct
) where

data Type   = Any   -- Oooooh, the "any" type. It makes some implementation details nicer later
            | Integer
            | Boolean
            | Unit
            | Ref Type
            | Fun Type Type -- Argument type, return type
            | Union Type Type -- Left type, right type
            | Product Type Type -- Left type, right type
            deriving (Show)

-- Ugly defn... "Any" matches any other type, the others match structurally identical types
instance Eq Type where
    Any == _ = True
    _ == Any = True
    Integer == Integer = True
    Boolean == Boolean = True
    Unit == Unit = True
    Ref t1 == Ref t2 = t1 == t2
    Fun lArg lRet == Fun rArg rRet = lArg == rArg && lRet == rRet
    Union lLeft lRight == Union rLeft rRight = lLeft == rLeft && lRight == rRight
    Product lLeft lRight == Product rLeft rRight = lLeft == rLeft && lRight == rRight
    _ == _ = False

isInteger :: Type -> Bool
isInteger = (== Integer)

isBoolean :: Type -> Bool
isBoolean = (== Boolean)

isUnit :: Type -> Bool
isUnit = (== Unit)

isRef :: Type -> Bool
isRef = (== Ref Any)

isFun :: Type -> Bool
isFun = (== Fun Any Any)

isUnion :: Type -> Bool
isUnion = (== Union Any Any)

isProduct :: Type -> Bool
isProduct = (== Product Any Any)