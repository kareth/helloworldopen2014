{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module GameInitModel where

import Data.Aeson ((.:), (.:?), decode, encode, (.=), object, FromJSON(..), ToJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))

-- Dimension

data Dimension = Dimension {
  dimensionLength   :: Float,
  width             :: Float,
  guideFlagPosition :: Float
} deriving (Show)

instance FromJSON Dimension where
  parseJSON (Object v) =
    Dimension <$>
    (v .: "length") <*>
    (v .: "width") <*>
    (v .: "guideFlagPosition")

-- CarId

data CarId = CarId {
  color     :: String,
  carIdName :: String
} deriving (Show)

instance FromJSON CarId where
  parseJSON (Object v) =
    CarId <$>
    (v .: "color") <*>
    (v .: "name")

-- Car

data Car = Car {
  carId         :: CarId,
  dimensions :: Dimension
} deriving (Show)

instance FromJSON Car where
  parseJSON (Object v) =
    Car <$>
    (v .: "id") <*>
    (v .: "dimensions")

-- Lane

data Lane = Lane {
  distanceFromCenter :: Int,
  index              :: Int
} deriving (Show)

instance FromJSON Lane where
  parseJSON (Object v) =
    Lane <$>
    (v .: "distanceFromCenter") <*>
    (v .: "index")

-- Piece

data Piece = Piece {
  pieceLength :: Maybe Float,
  switch :: Maybe Bool,
  radius :: Maybe Int,
  pieceAngle  :: Maybe Float,
  bridge :: Maybe Bool
} deriving (Show)

instance FromJSON Piece where
  parseJSON (Object v) =
    Piece <$>
    (v .:? "length") <*>
    (v .:? "switch") <*>
    (v .:? "radius") <*>
    (v .:? "angle")  <*>
    (v .:? "bridge")

-- StartingPoint

data StartingPoint = StartingPoint {
  position :: Position,
  angle    :: Float
} deriving (Show)

instance FromJSON StartingPoint where
  parseJSON (Object v) =
    StartingPoint <$>
    (v .: "position") <*>
    (v .: "angle")

-- Position

data Position = Position {
  x :: Float,
  y :: Float
} deriving (Show)

instance FromJSON Position where
  parseJSON (Object v) =
    Position <$>
    (v .: "x") <*>
    (v .: "y")

-- Track

data Track = Track {
  name          :: String,
  startingPoint :: StartingPoint,
  pieces        :: [Piece],
  lanes         :: [Lane]
} deriving (Show)

instance FromJSON Track where
  parseJSON (Object v) =
    Track <$>
    (v .: "name") <*>
    (v .: "startingPoint") <*>
    (v .: "pieces") <*>
    (v .: "lanes")

-- RaceSession

data RaceSession = RaceSession {
  laps :: Maybe Int,
  durationMs :: Maybe Int,
  maxLapTimeMs :: Maybe Int,
  quickRace :: Maybe Bool
} deriving (Show)

instance FromJSON RaceSession where
  parseJSON (Object v) =
    RaceSession <$>
    (v .:? "laps") <*>
    (v .:? "durationMs") <*>
    (v .:? "maxLapTimeMs") <*>
    (v .:? "quickRace")

-- Race

data Race = Race {
  track :: Track,
  cars  :: [Car],
  raceSession :: RaceSession
} deriving (Show)

instance FromJSON Race where
  parseJSON (Object v) =
    Race <$>
    (v .: "track") <*>
    (v .: "cars") <*>
    (v .: "raceSession")

-- GameInitData

data GameInitData = GameInitData {
  race :: Race
} deriving (Show)

instance FromJSON GameInitData where
  parseJSON (Object v) =
    GameInitData <$>
    (v .: "race")

-- Helpers

players :: GameInitData -> [CarId]
players gameInit =
  map (\car -> carId $ car) $ cars $ race gameInit

piecesOfGame :: GameInitData -> [Piece]
piecesOfGame gameInit =
  pieces $ track $ race gameInit

lanesOfGame :: GameInitData -> [Lane]
lanesOfGame gameInit =
  lanes $ track $ race gameInit

reportGameInit :: GameInitData -> String
reportGameInit gameInit =
  "Players: " ++ (show $ players gameInit) ++ ", Track: " ++ show (length $ piecesOfGame gameInit) ++ " pieces"
