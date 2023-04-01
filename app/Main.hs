module Main where

import Control.Monad.Trans.Reader
import Control.Exception
import Control.Monad.IO.Class
import Servant
import Data.Functor
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Class
import StackTrace (StackTraceRef)
import StackTrace qualified 
import RIO
import Data.Function ((&))

newtype Foo m = Foo { runFoo :: m () }
newtype Bar m = Bar { runBar :: m () }
newtype Baz m = Baz { runBaz :: m () }

makeFoo :: Bar m -> Foo m
makeFoo bar = Foo { runFoo = runBar bar}

makeBar :: Baz m -> Bar m
makeBar baz = Bar { runBar = runBaz baz}

makeBaz :: Baz (RIO e)
makeBaz = Baz { runBaz = liftIO $ throwIO $ userError "some exception"}

type API = PostNoContent

type FooServer = ServerT API (RIO ())

makeFooServer :: Foo (RIO StackTraceRef) -> ServerT API (RIO StackTraceRef)
makeFooServer foo = runFoo foo $> NoContent

main :: IO ()
main = do
    let fooServer = makeFooServer foo
        foo = makeFoo bar & \Foo {runFoo} -> Foo { runFoo = StackTrace.annotate "runFoo" runFoo}
        bar = makeBar baz & \Bar {runBar} -> Bar { runBar = StackTrace.annotate "runBar" runBar}
        baz = makeBaz & \Baz {runBaz} -> Baz { runBaz = StackTrace.annotate "runBax" runBaz}
        t action = Servant.Handler $ lift $ StackTrace.with $ runReaderT action
    run 8000 $ serve (Proxy @API) $ hoistServer (Proxy @API) t fooServer
