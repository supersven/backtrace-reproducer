# Backtraces

Use GHC 9.10

## Usage 1: Warp's own `setOnException`:

```haskell
  […]
  let warpSettings =
        setPort 8085 $
          setOnException (onExceptionHandler)
            defaultSettings
  […]

onExceptionHandler :: Maybe Request -> SomeException -> IO ()
onExceptionHandler _mRequest exception = do
  backtraces <- collectBacktraces
  putStrLn $ "Exception: " <> show exception
  putStrLn $ displayBacktraces backtraces
```

```
Exception: libpq: failed (connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
        Is the server running locally and accepting connections on that socket?
)
IPE backtrace:
    Network.Wai.Handler.Warp.HTTP1.http1server.loop (./Network/Wai/Handler/Warp/HTTP1.hs:(142,13)-(157,42))
    Network.Wai.Handler.Warp.HTTP1.settingsOnException (./Network/Wai/Handler/Warp/Settings.hs:47:7-25)
    Server.onExceptionHandler (src/Server.hs:(37,1)-(40,41))
HasCallStack backtrace:
  collectBacktraces, called at src/Server.hs:38:17 in backtrace-0.1.0.0-inplace:Server
```

## Usage 2: Catch Your Own Exceptions

### IPE Backtraces (No interesting result)

```
Exception: libpq: failed (connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
        Is the server running locally and accepting connections on that socket?
)
IPE backtrace:
    Network.Wai.Handler.Warp.HTTP1.http1server.loop (./Network/Wai/Handler/Warp/HTTP1.hs:(142,13)-(157,42))
    Network.Wai.Handler.Warp.HTTP1.processRequest (./Network/Wai/Handler/Warp/HTTP1.hs:(195,10)-(203,31))
    Network.Wai.Handler.Warp.HTTP1.processRequest (./Network/Wai/Handler/Warp/HTTP1.hs:195:20-22)
    Control.Monad.Trans.Resource.runResourceT (./Control/Monad/Trans/Resource.hs:193:16-39)
    Servant.Server.Internal.Delayed.runHandler' (src/Servant/Server/Internal/Handler.hs:27:31-41)
    Server.naturalTransform (src/Server.hs:(45,22)-(46,39))
    Server. (:)
    Server.unsafeEff_ (src/Effectful/Internal/Monad.hs:157:1-34)
HasCallStack backtrace:
  collectBacktraces, called at src/Server.hs:57:32 in backtrace-0.1.0.0-inplace:Server
```
