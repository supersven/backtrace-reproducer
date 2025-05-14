# Backtraces

## Instructions

* Use GHC 9.10
* Commands:
  1. `cabal build`
  2. `cabal run`
  3. `curl http://localhost:8085/db`

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

#### Pretty-printed stack shortly before the failing function is called

```
GenStgStackClosure
  { ssc_info =
      StgInfoTable
        { entry = Nothing
        , ptrs = 0
        , nptrs = 0
        , tipe = STACK
        , srtlen = 0
        , code = Nothing
        }
  , ssc_stack_size = 4093
  , ssc_stack =
      [ RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 23415432
                , code = Nothing
                }
          , stack_payload = []
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x4200410ab8 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 2
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 36384272
                , code = Nothing
                }
          , stack_payload =
              [ StackBox 0x420040e808 / 1 , StackBox 0x420040f038 / 1 ]
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040f138 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 23453144
                , code = Nothing
                }
          , stack_payload = [ StackBox 0x420040f108 ]
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x1ac3fa8 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 991250
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 23458416
                , code = Nothing
                }
          , stack_payload =
              [ StackBox 0x420040add0
              , StackBox 0x4200408560 / 1
              , StackBox 0x1ab3300 / 3
              , StackBox 0x1ab3838 / 1
              , StackBox 0x1ab3968 / 2
              , StackBox 0x4200409f98 / 1
              , StackBox 0x420040b870 / 1
              , StackWord 27672146
              , StackBox 0x420040ae30 / 1
              , StackBox 0x420040c7d0
              , StackWord 283472087081
              , StackWord 0
              , StackWord 28132345
              , StackWord 13
              , StackBox 0x420040e6e8
              , StackBox 0x420040e510 / 1
              , StackBox 0x420040e1b8 / 2
              , StackBox 0x420040e258 / 1
              ]
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040e598 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 2067
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 23454984
                , code = Nothing
                }
          , stack_payload =
              [ StackBox 0x4200408560 / 1
              , StackBox 0x1ab3300 / 3
              , StackBox 0x1ab3838 / 1
              , StackBox 0x27a0058 / 1
              , StackBox 0x1ab3968 / 2
              , StackWord 51200
              , StackBox 0x4200409f98 / 1
              , StackBox 0x1ac16a8 / 2
              , StackBox 0x420040b870 / 1
              , StackBox 0x420040a540 / 2
              , StackBox 0x1ab94e0 / 1
              , StackBox 0x1a63e50 / 2
              , StackBox 0x420040ae30 / 1
              , StackBox 0x420040ae20 / 1
              , StackBox 0x420040c7d0
              , StackBox 0x420040c7e0
              , StackBox 0x420040a358 / 1
              , StackBox 0x420040c800 / 1
              , StackBox 0x420040c828 / 1
              ]
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040c840 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040b808 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 2
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload =
              [ StackBox 0x420040a648 / 2 , StackBox 0x27a0060 / 2 ]
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040b700 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 2
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload =
              [ StackBox 0x1be8ee8 / 2 , StackBox 0x420040ae30 / 1 ]
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x1be8f40 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload = []
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040ac98 / 2
          }
      , RetSmall
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 2
                , nptrs = 0
                , tipe = RET_SMALL
                , srtlen = 0
                , code = Nothing
                }
          , stack_payload =
              [ StackBox 0x1aabfe0 / 2 , StackBox 0x420040ac80 / 1 ]
          }
      , CatchFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 1
                , nptrs = 0
                , tipe = CATCH_FRAME
                , srtlen = 0
                , code = Nothing
                }
          , handler = 0x420040a608 / 2
          }
      , StopFrame
          { info_tbl =
              StgInfoTable
                { entry = Nothing
                , ptrs = 0
                , nptrs = 0
                , tipe = STOP_FRAME
                , srtlen = 0
                , code = Nothing
                }
          }
      ]
  }
```
