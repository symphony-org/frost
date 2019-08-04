module GitPluginSpec where

import Frost
import Frost.Plugin
import Frost.GitPlugin
import Frost.Effects.Git

import Text.Pandoc
import Polysemy
import Test.Hspec

spec :: Spec
spec =
  describe "GitPlugin" $ do
    it "should substitute frost code blocks with content from the git plugin" $ do
      -- given
      let blocks = [ CodeBlock ("",["frost:git:devs"],[]) ""]
      let pandoc = Pandoc nullMeta blocks
      -- when
      let res = run $ runGitPure ["Dev1", "Dev2"] $ substitute gitPlugin ""
      -- then
      res `shouldBe` [BulletList [ [Plain [Str "Dev1"]], [Plain [Str "Dev2"]]]]
