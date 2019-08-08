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
      let result = runStack clean
                 & runCmd ("stack --no-terminal clean", "cleaned")
                 & run
      result `shouldBe` "cleaned"
    it "build should call stack --no-terminal build" $ do
      let result = runStack build
                 & runCmd ("stack --no-terminal build", "built")
                 & run
      result `shouldBe` "built"
    it "exec should call stack --no-terminal exec <<program_name>>" $ do
      let result = runStack (exec "foo-exe")
                 & runCmd ("stack --no-terminal exec foo-exe", "result of exec")
                 & run
      result `shouldBe` "result of exec"
    it "test should call stack --no-terminal test" $ do
      let result = runStack test
                 & runCmd ("stack --no-terminal test", "result of test")
                 & run
      result `shouldBe` "result of test"

runCmd :: (String, String) -> Sem (Sys ': r) a -> Sem r a
runCmd (arg, response) program = runSysPure ts func program
  where
    ts = undefined
    func str = if str == arg then ("", response) else ("", "")
  

