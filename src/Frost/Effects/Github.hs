{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Frost.Effects.Github where

import Polysemy
import Polysemy.Error
import FrostError
import qualified GitHub.Endpoints.Issues as GHI
import qualified GitHub.Data.Options as GHO
import qualified GitHub.Data.Issues as GHDI
import PolysemyContrib
import qualified Data.Vector as DV

type Issue = String

data Github m a where
  Issues :: Github m [Issue]

makeSem ''Github

runGithubPure :: [Issue] -> Sem (Github ': r) a -> Sem r a
runGithubPure issues = interpret $ \case
  Issues -> return issues

runGithubIO :: (Member (Embed IO) r, Member (Error FrostError) r) => Sem (Github ': r) a  -> Sem r a
runGithubIO = interpret $ \case
  Issues -> fromEitherSem $ embed $ issuesForFrost
  where
    -- issuesForFrost :: IO (Either FrostError [Issue]) -- IO (Either Error (Vector Issue))
    issuesForFrost = do
      mis <- GHI.issuesForRepo "rabbitonweb" "frost" GHO.optionsAnyMilestone
      return $ either (\e -> Left $ FrostError $ show e) (\l -> Right $ fmap (\i -> show $ GHDI.issueTitle i) (DV.toList l) ) mis

