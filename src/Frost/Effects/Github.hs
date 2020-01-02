{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Frost.Effects.Github where

import qualified Data.List.Split         as DLS
import qualified Data.Vector             as DV
import           FrostError
import qualified GitHub.Data.Issues      as GHDI
import qualified GitHub.Data.Options     as GHO
import qualified GitHub.Endpoints.Issues as GHI
import qualified GitHub.Internal.Prelude as GIP
import           Polysemy
import           Polysemy.Error
import           PolysemyContrib

type Issue = String

data Github m a where
  Issues :: String -> Github m [Issue]

makeSem ''Github

runGithubPure :: [Issue] -> Sem (Github ': r) a -> Sem r a
runGithubPure issues = interpret $ \case
  Issues _ -> return issues

runGithubIO :: (Member (Embed IO) r, Member (Error FrostError) r) => Sem (Github ': r) a  -> Sem r a
runGithubIO = interpret $ \case
  Issues repo -> do
    (username, reponame) <- either throw return (parseRepo repo)
    fromEitherSem $ embed $ issuesForFrost username reponame
  where
    issuesForFrost username reponame = do
      mis <- GHI.issuesForRepo username reponame GHO.optionsAnyMilestone
      return $ either (Left . FrostError . show) (Right . fmap (GIP.unpack . GHDI.issueTitle) . DV.toList) mis
    parseRepo :: String -> Either FrostError (GHI.Name GHI.Owner, GHI.Name GHI.Repo)
    parseRepo d = case DLS.splitOn "/" d of
        [username, reponame] -> Right (GHI.mkOwnerName $ GIP.pack username, GHI.mkRepoName $ GIP.pack reponame)
        other -> Left $ FrostError $ show other
