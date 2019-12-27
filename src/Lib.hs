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
import Extensions
import qualified Data.Map.Strict as Map


type GetGame = "get" :> Capture "gameId" Int :> Get '[JSON] (Maybe Game)
type GetAllGames = "getAll" :> Get '[JSON] [Game]
type AddGame = "post" :> ReqBody '[JSON] Game :> Post '[JSON] Game
type ConnectToGame = "connectToGame" :> Capture "gameNumber" Int :> Post '[JSON] PlayerAndGameInfo
type CreateGame = "createGame" :> Capture "fieldSize" Int :> Post '[JSON] PlayerAndGameInfo
type StartGame = "startGame" :> Capture "gameNumber" Int :> Post '[JSON] ()
type CheckState = "checkState" :> Capture "gameNumber" Int :> Get '[JSON] ResponseForWhileTrue
type MakeTurn = "makeTurn" :> ReqBody '[JSON] MakeTurnChanges :> Post '[JSON] ()
type CheckIsGameEnded = "checkIsGameEnded" :> Capture "gameNumber" Int :> Get '[JSON] IsGameEnded
type API = 
  GetGame :<|>
  GetAllGames :<|>
  AddGame :<|>
  ConnectToGame :<|>
  CreateGame :<|>
  StartGame :<|>
  CheckState :<|>
  MakeTurn :<|>
  CheckIsGameEnded

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
  createGame :<|>
  startGame :<|>
  checkState :<|>
  makeTurn :<|>
  checkIsGameEnded

  where getGame :: Int -> AppM (Maybe Game)
        getGame findId = do
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          return $ findGame findId items

        getAllGames :: AppM [Game]
        getAllGames = do
          State{games = p} <- ask
          games <- liftIO $ atomically $ readTVar p
          return $ reverse $ returnOnlyAvailableGames games

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
          let newGame = Game (findLastId items) (ResponseForWhileTrue False False 1 1 (Changes 0 0 'n')) (Field size (reverse $ emptyBoard size))
          liftIO $ atomically $ readTVar p >>= writeTVar p . (newGame :)
          return $ PlayerAndGameInfo (gameId newGame) 1 size

        startGame :: Int -> AppM ()
        startGame gameNumber = do
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          let newItems = startOneGame gameNumber items
          let game = findGame gameNumber items
          case game of
              Nothing -> throwError err422
              Just gameObj -> do
                liftIO $ atomically $ swapTVar p newItems
                return ()

        checkState :: Int -> AppM ResponseForWhileTrue
        checkState gameNumber = do
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          let game = findGame gameNumber items
          case game of
              Nothing -> throwError err422
              Just (Game _ responseForWhileTrue _) -> return responseForWhileTrue

        makeTurn :: MakeTurnChanges -> AppM ()
        makeTurn changes = do
          let (MakeTurnChanges _ (PlayerAndGameInfo gameNumber playerNumber _)) = changes
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          let game = findGame gameNumber items
          case game of
              Nothing -> throwError err422
              Just gameObj -> do
                let check = isTurnAvailable (turnChanges changes) (gameField gameObj)
                if check == True
                  then do
                    let newItems = changeBoardState changes items
                    liftIO $ atomically $ swapTVar p newItems
                    return ()
                  else throwError err422

        checkIsGameEnded :: Int -> AppM IsGameEnded
        checkIsGameEnded gameNumber = do
          State{games = p} <- ask
          items <- liftIO $ atomically $ readTVar p
          let game = findGame gameNumber items
          case game of
              Nothing -> throwError err422
              Just gameObj -> do
                return $ IsGameEnded $ isGameEnded $ info gameObj

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
