module Frost.Plugin where

import Text.Pandoc
import Polysemy

data Plugin r = Plugin {
                     addToMeta :: Meta -> Sem r Meta
                     }
