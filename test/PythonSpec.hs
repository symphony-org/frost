{-# LANGUAGE TupleSections #-}
module PythonSpec where

import Frost.Effects.Sys
import Frost.Effects.Python

import Polysemy

import Data.Function               ((&))
import Test.Hspec

spec :: Spec
spec =
  describe "Frost.Effects.Python runPython" $ do
    it "run script should call 'python -c script'" $ do
      let result = runPython (exec "script")
                 & runCmd ("python -c 'script'", withStdOut "executed a python script")
                 & run
      result `shouldBe` "executed a python script"

runCmd :: (String, (StdOut, StdErr)) -> Sem (Sys ': r) a -> Sem r a
runCmd (arg, response) program = runSysPure ts func program
  where
    ts = undefined
    func str = if str == arg then response else ("", "")
    withStdOut :: String -> (StdOut, StdErr)
    withStdOut = (, "")
