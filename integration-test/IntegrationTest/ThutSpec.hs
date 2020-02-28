{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module IntegrationTest.ThutSpec where

import           Frost.Effects.Thut
import           FrostError

import           Polysemy
import           Polysemy.Error

import           Data.Function      ((&))
import           Test.Hspec

spec :: Spec
spec =
  describe "Frost.Effects.Thut runThutIO" $
    it "should execute thut:passthrough" $ do
      pendingWith "make it work"
      result <- runThutIO (passthrough "putStrLn \"Hello world!\"")
                & runError @FrostError
                & runM
      result `shouldBe` Right "Hello world!\n"
