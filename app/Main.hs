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
import Control.Monad.IO.Unlift

newtype Foo m = Foo { runFoo :: m () }
newtype Bar m = Bar { runBar :: m () }
newtype Baz m = Baz { runBaz :: m () }

makeFoo :: Bar m -> Foo m
makeFoo bar = Foo { runFoo = runBar bar}

makeBar :: Baz m -> Bar m
makeBar baz = Bar { runBar = runBaz baz}

-- makeBar :: Baz (RIO e) -> Bar (RIO e)
-- makeBar baz = Bar { runBar = do 
--         withRunInIO $ \runInIO -> do
--             _ <- try @IOException $ runInIO (runBaz baz)
--             pure ()
--         liftIO $ throwIO (userError "should be last in the stack trace, but it's not")
--     }

makeBaz :: Baz (RIO e)
makeBaz = Baz { runBaz = liftIO $ throwIO $ userError "some exception"}

type API = PostNoContent

makeFooServer :: Foo (RIO StackTraceRef) -> ServerT API (RIO StackTraceRef)
makeFooServer foo = runFoo foo $> NoContent

main :: IO ()
main = do
    let fooServer = makeFooServer foo
        foo = makeFoo bar & \Foo {runFoo} -> Foo { runFoo = StackTrace.annotate "runFoo" runFoo}
        bar = makeBar baz & \Bar {runBar} -> Bar { runBar = StackTrace.annotate "runBar" runBar}
        baz = makeBaz & \Baz {runBaz} -> Baz { runBaz = StackTrace.annotate "runBax" runBaz}
        hoistRequest action = Servant.Handler $ lift $ StackTrace.with $ runReaderT action
    run 8000 $ serve (Proxy @API) $ hoistServer (Proxy @API) hoistRequest fooServer
