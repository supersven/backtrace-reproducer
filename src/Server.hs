module Server where

import Network.Wai
import Data.Function ((&))
import Effectful.Exception qualified as E
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import Control.Exception.Backtrace
import Servant.API
import Data.Text (Text, pack)
import GHC.Stack
import GHC.Generics
import Servant
import Servant.Server.Generic
import Effectful qualified as Eff
import Effectful (Eff, IOE)

data Routes mode = Routes
  { index :: mode :- Get '[PlainText] Text
  , getDB :: mode :- "db" :> Get '[PlainText] Text
  }
  deriving stock (Generic)

type ServerRoutes = NamedRoutes Routes

runServer :: IO ()
runServer = do
  setBacktraceMechanismState IPEBacktrace True
  setBacktraceMechanismState ExecutionBacktrace True
  let warpSettings = setPort 8085 $ defaultSettings
      server = mkServer
  runSettings warpSettings server

mkServer :: Application
mkServer =
  serveWithContextT
  (Proxy @ServerRoutes)
  EmptyContext
  (naturalTransform)
  (runApp)

naturalTransform :: Eff '[IOE] a -> Handler a
naturalTransform action = do
  result <- liftIO $ Right <$> action
      & (`E.catches` exceptionHandlers)
      & Eff.runEff
  either throwError pure result
  where
    exceptionHandlers =
      [E.Handler $ \(E.SomeException exception) -> do
          backtraces <- liftIO collectBacktraces
          liftIO $ putStrLn $ "Exception: " <> E.displayException exception
          liftIO $ putStrLn $ displayBacktraces backtraces
          pure $ Left err500 
      ]

runApp :: Routes (AsServerT (Eff '[IOE]))
runApp = Routes
  { index = handleIndex
  , getDB = handleDB
  }

handleIndex :: Eff '[IOE] Text
handleIndex = do
  error "meh"
  pure (pack "Hello")

handleDB :: Eff '[IOE] Text
handleDB = do
  conn <- liftIO $ connectPostgreSQL ""
  _results :: [Only Int] <- liftIO $ query_ conn "select 2 + 2"
  pure (pack "DB")
