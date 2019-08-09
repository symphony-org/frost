{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Github where

import Polysemy

type Issue = String

data Github m a where
  Issues :: Github m [Issue]

makeSem ''Github

runGithubPure :: [Issue] -> Sem (Github ': r) a -> Sem r a
runGithubPure issues = interpret $ \case
  Issues -> return issues