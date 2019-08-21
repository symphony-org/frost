module Plugins.LatestIssuesPluginSpec where

import Frost
import Frost.Plugin
import Frost.Plugins.LatestIssuesPlugin
import Frost.Effects.Github

import Data.Function               ((&))
import Text.Pandoc
import Polysemy
import Test.Hspec

spec :: Spec
spec =
  describe "LatestIssuesPlugin" $ do
    it "should substitute frost code blocks with content from the github effect" $ do
      -- when
      let res = substitute latestIssuesPlugin ""
            & runGithubPure ["The best issue ever", "The second best issue ever", "One other issue", "YAI"]
            & run
      -- then
      fst res `shouldBe` [BulletList [ [Plain [Str "The best issue ever"]], [Plain [Str "The second best issue ever"]], [Plain [Str "One other issue"]], [Plain [Str "YAI"]] ]]
      snd res `shouldBe` [Str "The best issue ever", Str "The second best issue ever", Str "One other issue", Str "YAI"]
