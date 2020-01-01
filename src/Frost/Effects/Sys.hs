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

type StdOut = String
type StdErr = String

data Sys m a where
  CurrentTime :: Sys m UTCTime
  Cmd :: String -> Sys m (StdOut, StdErr)

makeSem ''Sys

runSysPure :: UTCTime -> (String -> (StdOut, StdErr)) -> Sem (Sys ': r) a -> Sem r a
runSysPure ct cmdFun = interpret $ \case
  CurrentTime -> return ct
  Cmd command -> return $ cmdFun command

runSysIO :: ( Member (Embed IO) r
          , Member (Error FrostError) r
          ) => Sem (Sys ': r) a -> Sem r a
runSysIO = interpret $ \case
  CurrentTime -> embed getCurrentTime
  Cmd command -> executeCommand command >>= \case
    Left error -> throw error
    Right output -> return output
  where
    executeCommand command = embed (getProcessOutput command <&> \case
      (_, _, ExitFailure i) -> Left $ ExitedWithFailure i
      (stdOut, stdErr, ExitSuccess) -> Right (stdOut, stdErr))

getProcessOutput :: String -> IO (StdOut, StdErr, ExitCode)
getProcessOutput command =
  do (_pIn, pOut, pErr, handle) <- runInteractiveCommand command
     exitCode <- waitForProcess handle
     stdOut   <- hGetContents pOut
     stdErr   <- hGetContents pErr
     return (stdOut, stdErr, exitCode)
