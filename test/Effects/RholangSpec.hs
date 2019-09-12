{-# LANGUAGE TupleSections #-}
module Effects.RholangSpec where

import Frost.Effects.Sys
import Frost.Effects.Rholang

import Polysemy

import Data.Function               ((&))
import Test.Hspec

spec :: Spec
spec =
  describe "Frost.Effects.Rholang runRholang" $ do
    it "run script should call 'rholang script'" $ do
      let result = runRholang (exec "script")
                 & runCmd ("rholang 'script'", withStdOut "executed a rholang script")
                 & run
      result `shouldBe` "executed a rholang script"

runCmd :: (String, (StdOut, StdErr)) -> Sem (Sys ': r) a -> Sem r a
runCmd (arg, response) program = runSysPure ts func program
  where
    ts = undefined
    func str = if str == arg then response else ("", "")

withStdOut :: String -> (StdOut, StdErr)
withStdOut = (, "")
