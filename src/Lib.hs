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

data Book 
  = Book {
  bookId :: Int,
  bookName :: String
}
  deriving (Eq, Show, Generic)
instance ToJSON Book
instance FromJSON Book

type GetBooks = "get" :> Capture "bookId" Int :> Get '[JSON] (Maybe Book)
type AddBook = "post" :> ReqBody '[JSON] Book :> PostCreated '[JSON] Book
type BooksAPI = (GetBooks :<|> AddBook)

api :: Proxy BooksAPI
api = Proxy

data State = State
  { books :: TVar [Book]
  }

type AppM = ReaderT State Handler

server :: ServerT BooksAPI AppM
server = getBooks :<|> addBook
  where getBooks :: Int -> AppM (Maybe Book)
        getBooks findId = do
          State{books = p} <- ask
          item <- liftIO $ atomically $ readTVar p
          return $ findBook findId item

        addBook :: Book -> AppM Book
        addBook book = do
          State{books = p} <- ask
          liftIO $ atomically $ readTVar p >>= writeTVar p . (book :)
          return book

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

findBook :: Int -> [Book] -> Maybe Book
findBook _ [] = Nothing
findBook findId (book:books)
  | findId == (bookId book) = Just book
  | otherwise = findBook findId books