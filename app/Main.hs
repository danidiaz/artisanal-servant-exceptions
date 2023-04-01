module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Function ((&))
import Data.Functor
import Network.Wai.Handler.Warp
import RIO
import Servant
import StackTrace (StackTraceRef)
import StackTrace qualified

newtype Foo m = Foo {runFoo :: m ()}

newtype Bar m = Bar {runBar :: m ()}

newtype Baz m = Baz {runBaz :: m ()}

makeFoo :: Bar m -> Foo m
makeFoo bar = Foo {runFoo = runBar bar}

makeBar :: Baz m -> Bar m
makeBar baz = Bar {runBar = runBaz baz}

-- | A defect of this approach. 
-- 
-- If we catch an exception coming from an annotated function (here 'runBaz')
-- and then throw an unrelated exception _without crossing an annotation
-- frontier_ the old exception stays in the stack trace. 
makeBar' :: Baz (RIO e) -> Bar (RIO e)
makeBar' baz =
  Bar
    { runBar = do
        withRunInIO $ \runInIO -> do
          _ <- try @IOException $ runInIO (runBaz baz)
          throwIO (userError "should be last in the stack trace, but it isn't")
    }

makeBaz :: Baz (RIO e)
makeBaz = Baz {runBaz = liftIO $ throwIO $ userError "some exception"}

type API = PostNoContent

makeFooServer :: Foo (RIO StackTraceRef) -> ServerT API (RIO StackTraceRef)
makeFooServer foo = runFoo foo $> NoContent

main :: IO ()
main = do
  let fooServer = makeFooServer foo
      foo = makeFoo bar & \Foo {runFoo} -> Foo {runFoo = StackTrace.annotate "runFoo" runFoo}
      bar = makeBar baz & \Bar {runBar} -> Bar {runBar = StackTrace.annotate "runBar" runBar}
      -- bar = makeBar' baz & \Bar {runBar} -> Bar {runBar = StackTrace.annotate "runBar" runBar}
      baz = makeBaz & \Baz {runBaz} -> Baz {runBaz = StackTrace.annotate "runBaz" runBaz}
      hoistRequest action = Servant.Handler $ lift $ StackTrace.with $ runReaderT action
  run 8000 $ serve (Proxy @API) $ hoistServer (Proxy @API) hoistRequest fooServer
