module Frost.GitContributorsPlugin where

import Frost.Plugin
import Frost.Effects.Git

import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

gitContributorsPlugin :: Member Git r => Plugin r
gitContributorsPlugin = justContentPlugin "git:devs" (\_ -> render <$> devsList)
  where
    render devs = (renderContent devs, renderText devs)
    renderContent devs = [BulletList $ fmap (\d -> [Plain [Str d]]) devs]
    renderText = fmap Str
