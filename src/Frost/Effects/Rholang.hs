{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Rholang where

import           Data.Text
import           Frost.Effects.Sys
import           Polysemy

data Rholang m a where
  Exec :: Text -> Rholang m Text 

makeSem ''Rholang

runRholang :: Member Sys r => Sem (Rholang ': r) a -> Sem r a
runRholang = interpret $ \case
  Exec script -> showStdOut (cmd $ "rholang '" <> script <> "'")
  where
    showStdOut :: Sem r (Text, Text) -> Sem r Text 
    showStdOut = fmap fst
