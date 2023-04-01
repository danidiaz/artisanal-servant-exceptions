module StackTrace (StackTraceRef, with, annotate) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Data.IORef
import RIO
import System.IO

type Annotation = String

type StackTrace = [Annotation]

type StackTraceRef = IORef StackTrace

-- | Given some 'IO' action which works with an 'StackTraceRef', allocate the
-- ref and pass it to the action.
--
-- Additionally, catch any outgoing synchronous exception and re-throw it as an
-- 'ExceptionWithStackTrace' enriched with the contents of the 'StackTraceRef'.
with :: (StackTraceRef -> IO r) -> IO r
with f = do
  hPutStrLn stderr "Allocating stack trace ref"
  ref <- newIORef []
  er <- try @SomeException (f ref)
  case er of
    Left exception -> case fromException @SomeAsyncException exception of
      Just _ -> do
        -- Asynchronous exceptions pass through undecorated.
        throwIO exception
      Nothing -> do
        stackTrace <- readIORef ref
        throwIO (ExceptionWithStackTrace stackTrace exception)
    Right r -> pure r

-- | Given an annotation and an action, add the annotation to the stack trace if
-- we exit the action with an exception.
--
-- Clear the stack trace when entering the action and exiting without exception.
annotate :: Annotation -> RIO StackTraceRef a -> RIO StackTraceRef a
annotate frame action = do
  ref <- ask
  let clear = liftIO $ modifyIORef' ref (const [])
      add = liftIO $ modifyIORef' ref (frame :)
  clear
  r <- withRunInIO $ \runInIO ->
    runInIO action `onException` runInIO add
  clear
  pure r

data ExceptionWithStackTrace
  = ExceptionWithStackTrace StackTrace SomeException
  deriving (Show)

instance Exception ExceptionWithStackTrace
