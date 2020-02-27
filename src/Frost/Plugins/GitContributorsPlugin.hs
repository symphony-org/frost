module Frost.Plugins.GitContributorsPlugin where

import           Frost.Effects.Git
import           Frost.Plugin

import           Data.Map.Strict
import           Polysemy
import           PolysemyContrib
import           Text.Pandoc

gitContributorsPlugin :: Member Git r => Plugin r
gitContributorsPlugin = justContentPlugin "git:devs" (\_ -> render <$> devsList)
  where
    render devs = (renderContent devs, renderText devs)
    renderContent devs = [BulletList $ fmap (\d -> [Plain [Str d]]) devs]
    renderText = fmap Str
