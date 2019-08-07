module Frost.Plugin where

import Text.Pandoc
import Polysemy

data Plugin r = Plugin {
                     pluginName :: String,
                     substitute :: String -> Sem r ([Block], [Inline]),
                     addToMeta :: Meta -> Sem r Meta
                     }

justContentPlugin :: String -> (String -> Sem r ([Block], [Inline])) -> Plugin r
justContentPlugin pluginName substitute = Plugin pluginName substitute return

justMetaPlugin :: String -> (Meta -> Sem r Meta) -> Plugin r
justMetaPlugin pluginName addToMeta = Plugin pluginName (\_ -> return ([], [])) addToMeta
