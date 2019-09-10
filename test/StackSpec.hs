{-# LANGUAGE TupleSections #-}
module StackSpec where

import Frost.Effects.Sys
import Frost.Effects.Stack

import Polysemy

import Data.Function               ((&))
import Test.Hspec

spec :: Spec
spec =
  describe "Frost.Effects.Stack runStack" $ do
    it "clean should call stack --no-terminal clean" $ do
      let result = runStackSys clean
                 & runCmd ("stack --no-terminal clean", withStdErr "cleaned")
                 & run
      result `shouldBe` "cleaned"
    it "build should call stack --no-terminal build" $ do
      let result = runStackSys build
                 & runCmd ("stack --no-terminal build", withStdErr "built")
                 & run
      result `shouldBe` "built"
    it "exec should call stack --no-terminal exec <<program_name>>" $ do
      let result = runStackSys (exec "foo-exe")
                 & runCmd ("stack --no-terminal exec foo-exe", withStdErr "result of exec")
                 & run
      result `shouldBe` "result of exec"
    it "test should call stack --no-terminal test" $ do
      let result = runStackSys test
                 & runCmd ("stack --no-terminal test", withStdOut "result of test")
                 & run
      result `shouldBe` "result of test"
    it "testMatch should call stack --no-terminal test --match \"/<<filename>>/<<specname>>/<<testname>>/\"" $ do
      let result = runStackSys (testMatch "boo" "foo" "bar")
                 & runCmd ("stack --no-terminal test --match \"/boo/foo/bar/\"", withStdOut "result of test")
                 & run
      result `shouldBe` "result of test"

runCmd :: (String, (StdOut, StdErr)) -> Sem (Sys ': r) a -> Sem r a
runCmd (arg, response) program = runSysPure ts func program
  where
    ts = undefined
    func str = if str == arg then response else ("", "")

withStdOut :: String -> (StdOut, StdErr)
withStdOut = (, "")

withStdErr :: String -> (StdOut, StdErr)
withStdErr = ("", )

