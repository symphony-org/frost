module Frost.Plugin where

import Text.Pandoc
import Polysemy

data Plugin r = Plugin {
                     pluginName :: String,
                     substitute :: String -> Sem r [Block],
                     addToMeta :: Meta -> Sem r Meta
                     }
