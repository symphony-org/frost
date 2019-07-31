{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Git where

import Data.String.Utils
import Data.List
import Polysemy
import SimpleCmd.Git

import qualified Data.Text as T

data GitEffect m a where
  DevsList :: GitEffect m [String]

makeSem ''GitEffect

runGitEffect :: Member (Lift IO) r => Sem (GitEffect ': r) a -> Sem r a
runGitEffect = interpret $ \case
  DevsList -> sendM getContributors

getContributors :: IO [String]
getContributors  = do
    output <- git "log" ["--pretty=short", "-s"]
    let s = T.split (=='\n') (T.pack output)
    let r = nub $ filter (\x -> startswith  "Author:" (T.unpack x)) s
    let f = map (\x -> drop (length "Author: ") (T.unpack x)) r
    return $ f
