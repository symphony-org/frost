{-# LANGUAGE TemplateHaskell #-}
module PolysemyContrib where

import Polysemy
import Polysemy.Error
import Polysemy.State
import Data.Time.Clock

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map

fromEitherSem :: Member (Error e) r => Sem r (Either e a) -> Sem r a
fromEitherSem sem = sem >>= either throw (\b -> return b)

data FileProvider m a where
  ReadFile :: FilePath -> FileProvider m T.Text
  WriteFile :: FilePath -> T.Text -> FileProvider m ()

makeSem ''FileProvider

type InMemFileSystem = Map FilePath T.Text

runFileProviderPure :: (Member (State InMemFileSystem) r) => Sem (FileProvider ': r) a -> Sem r a
runFileProviderPure = interpret $ \case
  ReadFile path -> do
    m <- get @InMemFileSystem
    return $ m ! path
  WriteFile path content -> do
    m <- get @InMemFileSystem
    put $ insert path content m

runFileProviderIO :: (Member (Lift IO) r) => Sem (FileProvider ': r) a -> Sem r a
runFileProviderIO = interpret $ \case
  ReadFile path -> sendM $ TIO.readFile path
  WriteFile path content -> sendM $ TIO.writeFile path content

data SystemEffect m a where
  CurrentTime :: SystemEffect m UTCTime
  
makeSem ''SystemEffect

runSystemEffect :: Member (Lift IO) r => Sem (SystemEffect ': r) a -> Sem r a
runSystemEffect = interpret $ \case
  CurrentTime -> sendM getCurrentTime
