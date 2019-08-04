module Frost.GitPlugin where

import Frost.Plugin
import Frost.Effects.Git

import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict

gitPlugin :: Member Git r => Plugin r
gitPlugin = justContentPlugin "git:devs" (\_ -> render <$> devsList)
  where
    render devs = [BulletList $ fmap (\d -> [Plain [Str d]]) devs]
