{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Python where

import           Data.Text
import           Frost.Effects.Sys
import           Polysemy

data Python m a where
  Exec :: Text -> Python m Text 

makeSem ''Python

runPython :: Member Sys r => Sem (Python ': r) a -> Sem r a
runPython = interpret $ \case
  Exec script -> showStdOut (cmd $ "python -c '" <> script <> "'")
  where
    showStdOut :: Sem r (Text, Text) -> Sem r Text 
    showStdOut = fmap fst
