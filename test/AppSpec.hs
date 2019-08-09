{-# LANGUAGE QuasiQuotes #-}
module AppSpec where

import App
import Frost.Effects.FileProvider

import Polysemy hiding (raise)
import Polysemy.Input
import Polysemy.Error
import Polysemy.State
import PolysemyContrib

import Text.Pandoc
import Data.Map
import Data.Function ((&))
import qualified Data.Text as T
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec =
  describe "Frost.App mainString" $ do
    it "should handle error" $ do
      Left(error) <- mainString "unknownfile.md"
      let errorText = show error
      errorText `shouldBe` "Couldn't parse it :-("
