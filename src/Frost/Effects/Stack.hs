{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Stack where

import FrostError
import Frost.Effects.Sys

import Polysemy
import Polysemy.Error

type SpecName = String
type TestName = String
type FileName = String

data Stack m a where
  Clean :: Stack m String
  Build :: Stack m String
  Exec :: String -> Stack m String
  Test :: Stack m String
  TestMatch :: FileName -> SpecName -> TestName -> Stack m String

makeSem ''Stack

runStack :: Member Sys r => Sem (Stack ': r) a -> Sem r a
runStack = interpret $ \case
  Clean                       -> stack "clean"
  Build                       -> stack "build"
  Exec what                   -> stack $ "exec " ++ what
  Test                        -> stack "test"
  TestMatch fileName specName testName -> do
    let pattern = "/"++ fileName ++ "/" ++ specName ++ "/" ++ testName ++ "/"
    stack $ "test --match " ++ (show pattern)
  where
    stack :: Member Sys r => String -> Sem r String
    stack arg = fmap snd (cmd $ "stack --no-terminal " ++ arg)
