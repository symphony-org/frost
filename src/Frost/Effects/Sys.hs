{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Sys where

import Polysemy
import Data.Time.Clock


data Sys m a where
  CurrentTime :: Sys m UTCTime
  
makeSem ''Sys

runSys :: Member (Lift IO) r => Sem (Sys ': r) a -> Sem r a
runSys = interpret $ \case
  CurrentTime -> sendM getCurrentTime
