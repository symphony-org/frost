{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Stack where

import           Frost.Effects.Sys
import           FrostError

import           Polysemy
import           Polysemy.Error

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

runStackSys :: Member Sys r => Sem (Stack ': r) a -> Sem r a
runStackSys = interpret $ \case
  Clean                       -> showStdErr $ stack "clean"
  Build                       -> showStdErr $ stack "build"
  Exec what                   -> showStdErr $ stack $ "exec " ++ what
  Test                        -> showStdOut $ stack "test"
  TestMatch fileName specName testName -> do
    let p = "/"++ fileName ++ "/" ++ specName ++ "/" ++ testName ++ "/"
    showStdOut $ stack $ "test --match " ++ show p
  where
    stack :: Member Sys r => String -> Sem r (StdOut, StdErr)
    stack arg = cmd $ "stack --no-terminal " ++ arg

    showStdOut :: Sem r (StdOut, StdErr) -> Sem r StdOut
    showStdOut = fmap fst

    showStdErr :: Sem r (StdOut, StdErr) -> Sem r StdErr
    showStdErr = fmap snd
