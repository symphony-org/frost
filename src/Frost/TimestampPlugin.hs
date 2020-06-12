module Frost.TimestampPlugin where

import           Frost.Effects.Sys
import           Frost.Plugin
import           Data.Text
import           Data.Functor
import           Data.Map.Strict
import           Polysemy
import           Text.Pandoc

timestampMetaPlugin :: Member Sys r => Plugin r
timestampMetaPlugin = justMetaPlugin "timestamp:meta" (\meta -> do
  time <- currentTime
  return $ Meta $ insertTimestamp time $ unMeta meta)
  where
    insertTimestamp t= insert "creation" (MetaString $ pack $ show t)

timestampPlugin :: Member Sys r => Plugin r
timestampPlugin = justContentPlugin "timestamp" (\_ -> currentTime <&> render)
  where
    render t = ([Plain [Str $ pack $ show t]], [Str $ pack $ show t])
