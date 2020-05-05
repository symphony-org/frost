module PythonSpec where

import           Frost.Effects.Python
import           Frost.Effects.Sys
import           FrostError

import           Polysemy
import           Polysemy.Error

import           Data.Function        ((&))
import           Test.Hspec

spec :: Spec
spec =
  describe "Frost.Effects.Python runPython" $
    it "should execute python" $ do
      result <- runPython (exec "print(1+3)")
                & runSysIO
                & runError @FrostError
                & runM
      result `shouldBe` Right "4\n"
