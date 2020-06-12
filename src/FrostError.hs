module FrostError where

import Data.Text

data FrostError = PluginNotAvailable Text | ExitedWithFailure Int | FrostError String
  deriving (Eq, Show)
