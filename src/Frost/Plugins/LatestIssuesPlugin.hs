
module Frost.Plugins.LatestIssuesPlugin where

import           Data.List
import           Data.Map.Strict
import           Frost.Effects.Github
import           Frost.Plugin
import           Polysemy
import           PolysemyContrib
import           Text.Pandoc

latestIssuesPlugin :: (Member Github r) => Plugin r
latestIssuesPlugin = justContentPlugin "issues:latest" (\repo -> do
      is <- issues repo
      return (renderBlock is, renderInline is))
  where
    renderBlock is = [BulletList (fmap (wrap . Plain . wrap . Str) is)]
    renderInline = fmap Str
    wrap :: c -> [c]
    wrap = return
