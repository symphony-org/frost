module GitContributorsPluginSpec where

import Frost
import Frost.Plugin
import Frost.GitContributorsPlugin
import Frost.Effects.Git

import Text.Pandoc
import Polysemy
import Test.Hspec

spec :: Spec
spec =
  describe "GitContributorsPlugin" $ do
    it "should substitute frost code blocks with content from the git plugin" $ do
      -- when
      let res = run $ runGitPure ["Dev1", "Dev2"] $ substitute gitContributorsPlugin ""
      -- then
      res `shouldBe` [BulletList [ [Plain [Str "Dev1"]], [Plain [Str "Dev2"]]]]
