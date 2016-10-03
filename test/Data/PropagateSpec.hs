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
  context "Monoid" $ do
    prop "left identity" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \xs ->
        xs `mappend` mempty == xs

    prop "right identity" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \xs ->
        mempty `mappend` xs == xs

    prop "associativity" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \xs ->
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \ys ->
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \zs ->
      mappend xs (mappend ys zs) === mappend (mappend xs ys) zs

    prop "mconcat" $
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \xs ->
      forAll (fromList <$> arbitrary :: Gen (PtQueue Char)) $ \ys ->
        mconcat [xs,ys] == foldr mappend mempty ([xs,ys] :: [PtQueue Char])

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
        fmap f xs `union` fmap f ys === fmap f (xs `union` ys)
      )))

  context "leftMerge" $ do
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

  context "accumulate" $ do
    specify "zero case" $ do
      let f = (\a x -> x : a) :: [Char] -> Char -> [Char]
      accumulate f [] ([] :: PtQueue Char) `shouldBe` []

    specify "hello world" $ do
      let f = (\a x -> a ++ [x])
          stream = [e 1 'H'
                   ,e 1 'e'
                   ,e 1 'l'
                   ,e 1 'l'
                   ,e 1 'o'
                   ,e 1 ' '
                   ,e 2 'W'
                   ,e 2 'o'
                   ,e 2 'r'
                   ,e 2 'l'
                   ,e 2 'd'
                   ] :: PtQueue Char
      accumulate f [] stream `shouldBe` "Hello World"

  context "delay" $ do
    specify "zero case" $ do
      delay ([] :: PtQueue Char) `shouldBe` []

    specify "one" $ do
      delay ([e 1 'a'] :: PtQueue Char) `shouldBe` []

    specify "two" $ do
      delay ([e 1 'a', e 2 'b'] :: PtQueue Char) `shouldBe` ([e 2 ('a','b')] :: PtQueue (Char,Char))

    prop "n" $ do
      forAll (arbitrary :: Gen [Char]) $ \xs -> do
        let eventPairs = [1..] `zip` xs
            delayedPairs = [2..] `zip` (xs `zip` (tail xs))
            stream = fromList [mkEvent (fromIntegral t) x | (t,x) <- eventPairs]
            res = fromList [mkEvent (fromIntegral t) x | (t,x) <- delayedPairs]
        delay stream `shouldBe` res

e = mkEvent

instance Arbitrary a => Arbitrary (PtEvent a) where
  arbitrary = e <$> (fromIntegral <$> (arbitrary :: Gen Integer))
                <*> arbitrary

pending_ _ = pending
pendingWith_ s _ = pendingWith s
