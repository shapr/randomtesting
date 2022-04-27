#!/usr/bin/env cabal

{- cabal:
build-depends: base
            , hedgehog
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_associative_int :: Property
prop_associative_int = property $ do
  a <- forAll $ Gen.int (Range.linear minBound maxBound)
  b <- forAll $ Gen.int (Range.linear minBound maxBound)
  c <- forAll $ Gen.int (Range.linear minBound maxBound)
  ((a * b) * c) === (a * (b * c))

prop_associative_float :: Property
prop_associative_float = property $ do
  a <- forAll $ Gen.float (Range.linearFrac (-100) (100))
  b <- forAll $ Gen.float (Range.linearFrac (-10000) (10000))
  c <- forAll $ Gen.float (Range.linearFrac 5 1000000)
  ((a * b) * c) === (a * (b * c))

prop_reflexive' :: Property
prop_reflexive' = property $ do
  a <- forAll $ Gen.float (Range.linearFrac (-100) (100))
  a === a

prop_reflexive :: Property
prop_reflexive = property $ do
  a <- forAll $ Gen.float (Range.linearFrac (-100) (100))
  (a / 0) === (a / 0)

main = do
  checkParallel $
    Group
      "Example"
      [ --("prop_associative_int", prop_associative_int),
        -- ("prop_reflexive'", prop_reflexive'),
        ("prop_reflexive", prop_reflexive),
        ("prop_associative_float", prop_associative_float)
      ]
