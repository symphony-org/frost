{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Git where

import           Data.List
import           Data.String.Utils
import           Polysemy
import           SimpleCmd.Git

import qualified Data.Text         as T
import           Data.Text (Text)

data Git m a where
  DevsList :: Git m [Text]

makeSem ''Git

runGitIO :: Member (Embed IO) r => Sem (Git ': r) a -> Sem r a
runGitIO = interpret $ \case
  DevsList -> embed getContributors

runGitPure :: [Text] -> Sem (Git ': r) a -> Sem r a
runGitPure devs = interpret $ \case
  DevsList -> return devs

getContributors :: IO [Text]
getContributors  = do
    output <- git "log" ["--pretty=short", "-s"]
    let s = T.split (=='\n') (T.pack output)
    let r = nub $ filter (startswith  "Author:" . T.unpack) s
    let f = fmap (T.pack . drop (length ("Author: " :: String)) . T.unpack) r
    return f
