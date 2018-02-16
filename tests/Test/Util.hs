module Test.Util
(
    alpha,
    alphaNum,
    integers,
    booleans,
    identifiers,
    pairsOf
) where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen
import Control.Applicative (liftA2)

import Data.Char (toLower)

makePair :: (a -> b) -> Gen a -> Gen (a, b)
makePair f gen = do
                x <- gen
                return (x, f x)

integers :: Gen (Integer, String)
integers = makePair show arbitrarySizedNatural

booleans :: Gen (Bool, String)
booleans = makePair (map toLower . show) chooseAny

alpha :: Gen Char
alpha = elements $ ['a'..'z'] ++ ['A'..'Z']

alphaNum :: Gen Char
alphaNum = oneof [alpha, elements ['0'..'9']]

identifiers :: Gen String
identifiers = do h <- alpha
                 t <- listOf alphaNum
                 return $ h:t

pairsOf :: Gen a -> Gen (a, a)
pairsOf g = liftA2 (,) g g