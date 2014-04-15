{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Network(connectTo, PortID(..))
import System.IO(hPutStrLn, hGetLine, hSetBuffering, BufferMode(..), Handle)
import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson(decode, FromJSON(..), fromJSON, parseJSON, eitherDecode, Value(..), (.:), Result(..))

import GameInitModel
import CarPositionsModel

type ClientMessage = String

joinMessage botname botkey = "{\"msgType\":\"join\",\"data\":{\"name\":\"" ++ botname ++ "\",\"key\":\"" ++ botkey ++ "\"}}"
throttleMessage amount = "{\"msgType\":\"throttle\",\"data\":" ++ (show amount) ++ "}"
pingMessage = "{\"msgType\":\"ping\",\"data\":{}}"

connectToServer server port = connectTo server (PortNumber (fromIntegral (read port :: Integer)))

main = do
  args <- getArgs
  case args of
    [server, port, botname, botkey] -> do
      run server port botname botkey
    _ -> do
      putStrLn "Usage: hwo2014bot <host> <port> <botname> <botkey>"
      exitFailure

run server port botname botkey = do
  h <- connectToServer server port
  hSetBuffering h LineBuffering
  hPutStrLn h $ joinMessage botname botkey
  handleMessages h

handleMessages h = do
  msg <- hGetLine h
  case decode (L.pack msg) of
    Just json ->
      let decoded = fromJSON json >>= decodeMessage in
      case decoded of
        Success serverMessage -> handleServerMessage h serverMessage
        Error s -> fail $ "Error decoding message: " ++ s
    Nothing -> do
      fail $ "Error parsing JSON: " ++ (show msg)

data ServerMessage = Join | GameInit GameInitData | CarPositions [CarPosition] | Unknown String

handleServerMessage :: Handle -> ServerMessage -> IO ()
handleServerMessage h serverMessage = do
  responses <- respond serverMessage
  forM_ responses $ hPutStrLn h
  handleMessages h

respond :: ServerMessage -> IO [ClientMessage]
respond message = case message of
  Join -> do
    putStrLn "Joined"
    return [pingMessage]
  GameInit gameInit -> do
    putStrLn $ "GameInit: " ++ (reportGameInit gameInit)
    return [pingMessage]
  CarPositions carPositions -> do
    return $ [throttleMessage 0.5]
  Unknown msgType -> do
    putStrLn $ "Unknown message: " ++ msgType
    return [pingMessage]

decodeMessage :: (String, Value) -> Result ServerMessage
decodeMessage (msgType, msgData)
  | msgType == "join" = Success Join
  | msgType == "gameInit" = GameInit <$> (fromJSON msgData)
  | msgType == "carPositions" = CarPositions <$> (fromJSON msgData)
  | otherwise = Success $ Unknown msgType

instance FromJSON a => FromJSON (String, a) where
  parseJSON (Object v) = do
    msgType <- v .: "msgType"
    msgData <- v .: "data"
    return (msgType, msgData)
  parseJSON x          = fail $ "Not an JSON object: " ++ (show x)
