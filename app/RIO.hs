module RIO (RIO) where

import Control.Monad.Trans.Reader

type RIO e = ReaderT e IO