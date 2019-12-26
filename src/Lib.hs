{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
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
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, swapTVar)
import Types
import qualified Data.Map.Strict as Map


type GetGame = "get" :> Capture "gameId" Int :> Get '[JSON] (Maybe Game)
type GetAllGames = "getAll" :> Get '[JSON] [Game]
type AddGame = "post" :> ReqBody '[JSON] Game :> Post '[JSON] Game
type ConnectToGame = "connectToGame" :> Capture "gameNumber" Int :> Post '[JSON] PlayerAndGameInfo
type CreateGame = "createGame" :> Capture "fieldSize" Int :> Post '[JSON] PlayerAndGameInfo
type API = 
  GetGame :<|>
  GetAllGames :<|>
  AddGame :<|>
  ConnectToGame :<|>
  CreateGame

api :: Proxy API
api = Proxy

data State = State
  {
  games :: TVar [Game]
  }

type AppM = ReaderT State Handler

server :: ServerT API AppM
server = 
  getGame :<|>
  getAllGames :<|>
  addGame :<|>
  connectToGame :<|>
  createGame

  where getGame :: Int -> AppM (Maybe Game)
        getGame findId = do
          State{games = p} <- ask
          item <- liftIO $ atomically $ readTVar p
          return $ findGame findId item

        getAllGames :: AppM [Game]
        getAllGames = do
          State{games = p} <- ask
          liftIO $ atomically $ readTVar p

        addGame :: Game -> AppM Game
        addGame game = do
          State{games = p} <- ask
          liftIO $ atomically $ readTVar p >>= writeTVar p . (game :)
          return game

        connectToGame :: Int -> AppM PlayerAndGameInfo
        connectToGame gameNumber = do
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          let newItems = changeGames gameNumber items
          let game = findGame gameNumber items
          case game of
              Nothing -> throwError err422
              Just gameObj -> do
                liftIO $ atomically $ swapTVar p newItems
                return $ PlayerAndGameInfo (gameId gameObj) 2 (size $ gameField gameObj)

        createGame :: Int -> AppM PlayerAndGameInfo
        createGame size = do
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          let newGame = Game (findLastId items) (ResponseForWhileTrue False 1 1) (Field size)
          liftIO $ atomically $ readTVar p >>= writeTVar p . (newGame :)
          return $ PlayerAndGameInfo (gameId newGame) 1 size

changeGames :: Int -> [Game] -> [Game]
changeGames _ [] = []
changeGames gameId ((Game id (ResponseForWhileTrue isStarted playersTurn playersCount) (Field size)):xs)
  | gameId == id = (Game id (ResponseForWhileTrue isStarted playersTurn (playersCount + 1)) (Field size)) : xs
  | otherwise = (Game id (ResponseForWhileTrue isStarted playersTurn playersCount) (Field size)) : changeGames gameId xs

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

findLastId :: [Game] -> Int
findLastId [] = 1
findLastId (game:games) = (+) 1 $ gameId game