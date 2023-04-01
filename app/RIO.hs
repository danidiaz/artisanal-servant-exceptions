module RIO (RIO) where

import Control.Monad.Trans.Reader

-- | Just a convenient type synonym
type RIO e = ReaderT e IO