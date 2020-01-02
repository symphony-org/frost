{-# LANGUAGE TupleSections #-}
module Effects.RholangSpec where

import           Frost.Effects.Rholang
import           Frost.Effects.Sys

import           Polysemy

import           Data.Function         ((&))
import           Test.Hspec

spec :: Spec
spec =
  describe "Frost.Effects.Rholang runRholang" $
    it "run script should call 'rholang script'" $ do
  let result = runRholang (exec "script")
             & runCmd ("rholang 'script'", withStdOut "executed a rholang script")
             & run
  result `shouldBe` "executed a rholang script"

runCmd :: (String, (StdOut, StdErr)) -> Sem (Sys ': r) a -> Sem r a
runCmd (arg, response) = runSysPure ts func
  where
    ts = undefined
    func str = if str == arg then response else ("", "")

withStdOut :: String -> (StdOut, StdErr)
withStdOut = (, "")
