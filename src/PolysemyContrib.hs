
module PolysemyContrib where

import Polysemy
import Polysemy.Error

fromEitherSem :: Member (Error e) r => Sem r (Either e a) -> Sem r a
fromEitherSem sem = sem >>= either throw return
