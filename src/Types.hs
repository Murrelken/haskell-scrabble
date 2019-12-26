{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import GHC.Generics


data Book 
  = Book {
  bookId :: Int,
  bookName :: String
} deriving (Eq, Show, Generic)
instance ToJSON Book
instance FromJSON Book

data Game
  = Game {
    gameId :: Int,
    info :: ResponseForWhileTrue,
    gameField :: Field
  } 
  deriving (Eq, Show, Generic)

instance ToJSON Game
instance FromJSON Game

data ResponseForWhileTrue
  = ResponseForWhileTrue {
    isGameStarted :: Bool,
    playerTurnNumber :: Int,
    playersCount :: Int,
    changes :: Changes
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseForWhileTrue
instance FromJSON ResponseForWhileTrue

data Changes
  = Changes {
    positionX :: Int,
    positionY :: Int,
    playerNumber :: Char
  }
  deriving (Eq, Show, Generic)

instance ToJSON Changes
instance FromJSON Changes

data Field
  = Field {
  size :: Int,
  board :: [String]
} deriving (Eq, Show, Generic)
instance ToJSON Field
instance FromJSON Field

data PlayerAndGameInfo
  = PlayerAndGameInfo {
    gameNumber :: Int,
    givenPlayerNumber :: Int,
    fieldSize :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerAndGameInfo
instance FromJSON PlayerAndGameInfo

data MakeTurnChanges
  = MakeTurnChanges {
    turnChanges :: Changes,
    turnInfo :: PlayerAndGameInfo
  }
  deriving (Eq, Show, Generic)

instance ToJSON MakeTurnChanges
instance FromJSON MakeTurnChanges