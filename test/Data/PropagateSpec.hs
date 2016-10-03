{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.PropagateSpec (spec) where

import Data.Char             (toUpper)
import Data.Propagate
import Data.Propagate.IsList ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Show.Functions   ()

default (Integer)

spec :: Spec
spec = parallel $ do
  context "Queue" $ do
    prop "left identity" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $
        \xs -> xs `union` empty == xs

    prop "right identity" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $
        \xs -> empty `union` xs == xs

    specify "commutativity" $
      pendingWith "The current implementation in lists doesn't support it"

    prop "associativity" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \xs ->
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \ys ->
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \zs ->
        (xs `union` ys) `union` zs == xs `union` (ys `union` zs)

  context "Functor" $ do
    specify "identity" $ do
      fmap id [e 1 'a'] `shouldBe` ([e 1 'a'] :: PtQueue Char)

    specify "increment" $ do
      let stream = [e 1 'a', e 2 'b', e 3 'c'] :: PtQueue Char
      fmap toUpper stream `shouldBe` [e 1 'A', e 2 'B', e 3 'C']

    prop "general map property" (
      forAll (fromList <$> arbitrary :: Gen (PtQueue Int)) (\xs ->
      forAll (fromList <$> arbitrary :: Gen (PtQueue Int)) (\ys -> do
        let f = (\x->x^(x*x)) :: (Int -> Int)
        fmap f xs `union` fmap f ys == fmap f (xs `union` ys)
      )))

  context "Left Merge" $ do
    prop "zero case" (
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) (\zs -> do
      let streamA = empty :: PtQueue Char
      (streamA `leftMerge` zs) `shouldBe` empty
      ))

    specify "with empty stream" $ do
      let stream = [e 1 'a', e 2 'b', e 3 'c'] :: PtQueue Char
          emptyStream = empty :: PtQueue Char
          streamRes = [e 1 ('a',empty), e 2 ('b',empty), e 3 ('c',empty)]
      (stream `leftMerge` emptyStream) `shouldBe` streamRes

    specify "with many elements" $ do
      let streamA = [e 1 'a', e 2 'b', e 3 'c'] :: PtQueue Char
          streamB = [e 1 'X', e 2 'Y', e 4 'Z'] :: PtQueue Char
          streamRes = [ e 1 ('a', [e 1 'X'])
                      , e 2 ('b', [e 1 'X', e 2 'Y'])
                      , e 3 ('c', [e 1 'X', e 2 'Y'])
                      ]
      (streamA `leftMerge` streamB) `shouldBe` streamRes

    specify "self merge" $ do
      let stream = [e 1 'a', e 2 'b', e 3 'c'] :: PtQueue Char
          streamRes = [ e 1 ('a', [e 1 'a'])
                      , e 2 ('b', [e 1 'a', e 2 'b'])
                      , e 3 ('c', [e 1 'a', e 2 'b', e 3 'c'])
                      ]
      (stream `leftMerge` stream) `shouldBe` streamRes

e = mkEvent

instance Arbitrary a => Arbitrary (PtEvent a) where
  arbitrary = e <$> (fromIntegral <$> (arbitrary :: Gen Integer))
                <*> arbitrary

pending_ _ = pending
pendingWith_ s _ = pendingWith s
