{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Rholang where

import           Frost.Effects.Sys
import           Polysemy

data Rholang m a where
  Exec :: String -> Rholang m String

makeSem ''Rholang

runRholang :: Member Sys r => Sem (Rholang ': r) a -> Sem r a
runRholang = interpret $ \case
  Exec script -> showStdOut (cmd $ "rholang '" ++ script ++"'")
  where
    showStdOut :: Sem r (String, String) -> Sem r String
    showStdOut = fmap fst
