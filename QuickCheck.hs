#!/usr/bin/env cabal

{- cabal:
build-depends: base
            , QuickCheck
-}
module Main where

import Test.QuickCheck

-- associative: ((a `op` b) `op` c) == (a `op` (b `op` c))
prop_associative_int :: Int -> Int -> Int -> Bool
prop_associative_int a b c = ((a * b) * c) == (a * (b * c))

prop_associative_float :: Float -> Float -> Float -> Bool
prop_associative_float x y z = ((x * y) * z) == (x * (y * z))

-- reflexive: is something equal to itself?
prop_reflexive :: Float -> Bool
prop_reflexive f = f == f

prop_reflexive_div :: Float -> Bool
prop_reflexive_div f = (f / 0) == (f / 0)

main = do
  quickCheck $ (withMaxSuccess 10000) prop_associative_float
  quickCheck $ (withMaxSuccess 10000) prop_reflexive_div
