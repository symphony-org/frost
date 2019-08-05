{-# LANGUAGE TemplateHaskell #-}
module Frost.Effects.Sys where

import FrostError

import Polysemy
import Polysemy.Error
import Data.Functor
import Data.Time.Clock
import System.IO ( hGetContents )
import System.Process ( runInteractiveCommand , waitForProcess)
import System.Exit ( ExitCode ( .. ) )

data Sys m a where
  CurrentTime :: Sys m UTCTime
  Cmd :: String -> Sys m String
  
makeSem ''Sys

runSysPure :: UTCTime -> (String -> String) -> Sem (Sys ': r) a -> Sem r a
runSysPure ct cmdFun = interpret $ \case
  CurrentTime -> return ct
  Cmd command -> return $ cmdFun command

runSysIO :: ( Member (Lift IO) r
          , Member (Error DynamicError) r
          ) => Sem (Sys ': r) a -> Sem r a
runSysIO = interpret $ \case
  CurrentTime -> sendM getCurrentTime
  Cmd command -> executeCommand command >>= \case
    Left error -> throw error
    Right output -> return output
  where
    executeCommand command = sendM (getProcessOutput command <&> \case
      (_, (ExitFailure i)) -> Left $ ExitedWithFailure i
      (output, ExitSuccess) -> Right output)

getProcessOutput :: String -> IO (String, ExitCode)
getProcessOutput command =
  do (_pIn, pOut, pErr, handle) <- runInteractiveCommand command
     exitCode <- waitForProcess handle
     output   <- hGetContents pOut
     return (output, exitCode)

