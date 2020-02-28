{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Frost
import           Frost.DefaultsMandatoryPlugin
import           Frost.Effects.FileProvider
import           Frost.Effects.Git
import           Frost.Effects.Python
import           Frost.Effects.Rholang
import           Frost.Effects.Stack
import           Frost.Effects.Sys
import           Frost.Effects.Thut
import           Frost.PandocRun                     (runInputPandoc,
                                                      runOutputPandoc)
import           Frost.Plugin
import           Frost.Plugins.GitContributorsPlugin
import           Frost.Plugins.RholangPlugin
import           Frost.Plugins.StackPlugins
import           Frost.Plugins.ThutPlugin
import           Frost.PythonPlugin
import           Frost.TimestampPlugin
import           FrostError

import           Data.Foldable                       (find)
import           Data.Function                       ((&))
import qualified Data.Text                           as T
import           Options.Generic
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace
import           PolysemyContrib
import           System.Environment                  (getArgs)
import           System.Exit
import           System.IO
import           Text.Pandoc                         (PandocError)

data Config a = Config
  { input    :: a ::: [FilePath]       <?> "Files from which documentation is generated. Accepts multiple values"
  , template :: a ::: Maybe FilePath   <?> "(optional) HTML template used to generate the documentation"
  , output   :: a ::: FilePath         <?> "Path to a file that will hold generated documentation"
  } deriving Generic

instance ParseRecord (Config Wrapped)
deriving instance Show (Config Unwrapped)

main :: IO ()
main = do
  config <- unwrapRecord "Frost - automatically generates documentation from your source code"
  exitCode <- generate config >>= handleEithers
  exit exitCode
  where
    exit ExitSuccess     = exitSuccess
    exit (ExitFailure 1) = exitFailure
    generate (Config filePaths templatePath outputFilePath) = generateDocs (transform plugins)
      & runInputPandoc filePaths
      & runOutputPandoc outputFilePath templatePath
      & runFileProviderIO
      & runPython
      & runRholang
      & runStackSys
      & runThutIO
      & runSysIO
      & runGitIO
      & traceToIO
      & runError @FrostError
      & runError @PandocError
      & runM
    handleEithers = either (handle) (either (handle) (const $ return ExitSuccess))
    handle error = hPutStrLn stderr (show error) >> return (ExitFailure 1)

plugins :: Members [Git, Python, Rholang, Sys, Stack, Thut] r  => [Plugin r]
plugins = [ timestampPlugin
          , timestampMetaPlugin
          , defaultsMandatoryPlugin
          , gitContributorsPlugin
          , pythonPlugin
          , rholangPlugin
          ] ++ stackPlugins ++ thutPlugins
