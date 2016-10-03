{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Usage.ExamplesSpec (spec) where

import Data.Propagate
import Data.Propagate.IsList ()
import Test.Hspec

spec :: Spec
spec = parallel $ do
  context "Random Examples" $ do
    specify "KwhSamples" $ do
      let kwhSamples = [e 1 100, e 2 120, e 3 150] :: PtQueue Integer
          kwhDeltas = delayWith (\x y -> y - x) kwhSamples
      kwhDeltas `shouldBe` [e 2 20, e 3 30]

    specify "Report" $ do
      let kwhSamples = [ e 1 (1, 100)
                       , e 2 (10,120)
                       , e 3 (20,150)] :: PtQueue (Integer,Integer)
          kwhs = delayWith (\(_,x) (_,y) -> y - x) kwhSamples
          kws = delayWith (\(t1,x) (t2,y) -> (fromIntegral (y - x)) / (fromIntegral (t2 - t1))) kwhSamples
          kwhReport = accumulate (+) 0 kwhs
          kwReport = accumulate (max) 0.0 kws :: Double
      kwhReport `shouldBe` 50
      kwReport `shouldBe` 3

e = mkEvent
