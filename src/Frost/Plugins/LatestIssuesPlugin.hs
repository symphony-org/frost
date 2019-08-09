{-# LANGUAGE TemplateHaskell #-}
module Frost.Plugins.LatestIssuesPlugin where

import Frost.Plugin
import Text.Pandoc
import Polysemy
import PolysemyContrib
import Data.Map.Strict
import Frost.Effects.Github

latestIssuesPlugin :: (Member Github r) => Plugin r
latestIssuesPlugin = Plugin "issues:latest" bl return
  where
    bl :: (Member Github r) => String -> Sem r [Block]
    bl _ = do
      is <- issues
      render is

    render is = return $ [BulletList [(fmap (Plain . wrap . Str . show) is)]]
    wrap :: c -> [c]
    wrap = return