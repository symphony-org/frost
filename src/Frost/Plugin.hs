module Frost.Plugin where

import Text.Pandoc
import Polysemy

data Plugin r = Plugin {
                     name :: String,
                     substitute :: Sem r [Block],
                     addToMeta :: Meta -> Sem r Meta
                     }
