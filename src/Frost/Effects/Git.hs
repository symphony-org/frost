{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Git where

import Data.String.Utils
import Data.List
import Polysemy
import SimpleCmd.Git

import qualified Data.Text as T

data Git m a where
  DevsList :: Git m [String]

makeSem ''Git

runGit :: Member (Lift IO) r => Sem (Git ': r) a -> Sem r a
runGit = interpret $ \case
  DevsList -> sendM getContributors

getContributors :: IO [String]
getContributors  = do
    output <- git "log" ["--pretty=short", "-s"]
    let s = T.split (=='\n') (T.pack output)
    let r = nub $ filter (startswith  "Author:" . T.unpack) s
    let f = fmap (drop (length "Author: ") . T.unpack) r
    return $ f
