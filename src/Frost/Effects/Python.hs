{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Python where

import           Frost.Effects.Sys
import           Polysemy

data Python m a where
  Exec :: String -> Python m String

makeSem ''Python

runPython :: Member Sys r => Sem (Python ': r) a -> Sem r a
runPython = interpret $ \case
  Exec script -> showStdOut (cmd $ "python -c '" ++ script ++ "'")
  where
    showStdOut :: Sem r (String, String) -> Sem r String
    showStdOut = fmap fst
