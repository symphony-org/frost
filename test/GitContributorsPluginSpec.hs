module GitContributorsPluginSpec where

import Frost
import Frost.Plugin
import Frost.GitContributorsPlugin
import Frost.Effects.Git

import Data.Function               ((&))
import Text.Pandoc
import Polysemy
import Test.Hspec

spec :: Spec
spec =
  describe "GitContributorsPlugin" $
    it "should substitute frost code blocks with content from the git plugin" $ do
  -- when
  let res = substitute gitContributorsPlugin ""
        & runGitPure ["Dev1", "Dev2"]
        & run
  -- then
  fst res `shouldBe` [BulletList [ [Plain [Str "Dev1"]], [Plain [Str "Dev2"]]]]
  snd res `shouldBe` [Str "Dev1", Str "Dev2"]
