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

data SystemEffect m a where
  CurrentTime :: SystemEffect m UTCTime
  
makeSem ''SystemEffect

runSystemEffect :: Member (Lift IO) r => Sem (SystemEffect ': r) a -> Sem r a
runSystemEffect = interpret $ \case
  CurrentTime -> sendM getCurrentTime
