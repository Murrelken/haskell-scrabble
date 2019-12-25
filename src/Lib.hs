{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson
import           GHC.Generics                (Generic)
import           Network.HTTP.Client         (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp

import           Servant
import           Servant.Client
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Control.Monad.IO.Class
import GHC.Generics
import Control.Monad.STM           (atomically)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Types

type GetGame = "get" :> Capture "gameId" Int :> Get '[JSON] (Maybe Game)
type AddGame = "post" :> ReqBody '[JSON] Game :> PostCreated '[JSON] Game
type API = (GetGame :<|> AddGame)

api :: Proxy API
api = Proxy

data State = State
  {
  games :: TVar [Game]
  }

type AppM = ReaderT State Handler

server :: ServerT API AppM
server = getBooks :<|> addBook
  where getBooks :: Int -> AppM (Maybe Game)
        getBooks findId = do
          State{games = p} <- ask
          item <- liftIO $ atomically $ readTVar p
          return $ findGame findId item

        addBook :: Game -> AppM Game
        addBook game = do
          State{games = p} <- ask
          liftIO $ atomically $ readTVar p >>= writeTVar p . (game :)
          return game

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  initialBooks <- atomically $ newTVar []
  runSettings settings $ app $ State initialBooks

findGame :: Int -> [Game] -> Maybe Game
findGame _ [] = Nothing
findGame findId (game:games)
  | findId == (gameId game) = Just game
  | otherwise = findGame findId games
