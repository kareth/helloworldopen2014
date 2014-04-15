{-# LANGUAGE OverloadedStrings #-}

-- TODO this is a duplicate

module SpawnModel where

import GameInitModel(CarId)

import Data.Aeson ((.:), (.:?), decode, encode, (.=), object, FromJSON(..), ToJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad        (liftM)

-- Spawn

data Spawn = Spawn {
  msgType :: String,
  dataVal :: CarId,
  gameId  :: String
} deriving (Show)

instance FromJSON Spawn where
  parseJSON (Object v) =
    Spawn <$>
    (v .: "msgType") <*>
    (v .: "data")    <*>
    (v .: "gameId")

parseSpawn json = (decode json :: Maybe Spawn)
