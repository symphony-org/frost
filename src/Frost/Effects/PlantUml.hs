{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Frost.Effects.PlantUml where

import           Frost.Effects.Sys
import           Polysemy
import           Polysemy.State

data PlantUml m a where
  GeneratePng :: String -> PlantUml m FilePath

makeSem ''PlantUml

newtype PlantUmlPngCounter  = PlantUmlPngCounter { unPlantUmlPngCounter :: Int }
  deriving (Show, Eq)

runPlantUmlIO ::
     Member Sys r
  => Member (State PlantUmlPngCounter) r
  => Sem (PlantUml ': r) a
  -> Sem r a
runPlantUmlIO = interpret $ \case
  GeneratePng content -> do
    counter <- gets unPlantUmlPngCounter
    let outputPath = "res" <> (show counter) <> ".png"
    modify increment
    cmd $ "echo \"" <> content <> "\" | plantuml -p test.pu >" <> outputPath
    pure outputPath
  where
    increment (PlantUmlPngCounter i) = PlantUmlPngCounter $ i+1
