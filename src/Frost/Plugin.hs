module Frost.Plugin where

import           Polysemy
import           Text.Pandoc
import           Data.Text

data Plugin r = Plugin {
                     pluginName :: Text,
                     substitute :: Text -> Sem r ([Block], [Inline]),
                     addToMeta  :: Meta -> Sem r Meta
                     }

justContentPlugin :: Text -> (Text -> Sem r ([Block], [Inline])) -> Plugin r
justContentPlugin pluginName substitute = Plugin pluginName substitute return

justMetaPlugin :: Text -> (Meta -> Sem r Meta) -> Plugin r
justMetaPlugin pluginName = Plugin pluginName (\_ -> return ([], []))
