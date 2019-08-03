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

data Sys m a where
  CurrentTime :: Sys m UTCTime
  
makeSem ''Sys

runSys :: Member (Lift IO) r => Sem (Sys ': r) a -> Sem r a
runSys = interpret $ \case
  CurrentTime -> sendM getCurrentTime
