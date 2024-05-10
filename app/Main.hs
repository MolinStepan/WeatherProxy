{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Data.Text                  as T
import           Data.Time.Clock.System
import           Data.Time.Format           (parseTimeM)
import           Network.HTTP.Client        (Manager (..),
                                             defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp   (run)
import qualified Options.Applicative        as Opt
import           System.Environment

import           ArgumentsParser
import           Data.Common
import           Data.Weather
import           OpenWeather.City
import           OpenWeather.WeatherRequest
import           ServedApi

-- http://api.openweathermap.org/geo/1.0/direct?q={City}&limit={num}&appid={API key}
-- http://api.openweathermap.org/data/2.5/weather?lat={lat}&lon={lon}&appid={API key}

inputPToLoc :: Int -> Manager -> APIKey -> InputPoint -> IO Location
inputPToLoc port man key i = case i of
  Loc loc -> return loc
  City cit -> do
    ans <- getCityLocation port man key cit
    case ans of
      Right loc -> return loc
      Left err  -> error $ "error requesting " ++ T.unpack cit ++ ": " ++ show err

main :: IO ()
main = do
  options <- readComLineArgs -- Moscow "(23.21,122.11)" Chelyabinsk

  apiKey <- T.pack <$> getEnv "WEATHERPROXYAPIKEY"
  apiPort <- read <$> getEnv "WEATHERPROXYOPENWEATHERPORT"
  Just timeBetweenRequests <- readTime <$> getEnv "WEATHERPROXYINTERVAL"
  port <- read <$> getEnv "WEATHERPROXYPORT"
  timeError' <- lookupEnv "WEATHERPROXYTIMEERROR"
  let Just timeError = (readTime =<< timeError') <|> Just 900
  locError' <- lookupEnv "WEATHERPROXYLOCATIONERROR"
  let Just locError = (read <$> locError') <|> Just 0.001

  apiManager <- newManager defaultManagerSettings
  MkSystemTime utc _ <- getSystemTime
  listOfLocs <- traverse (inputPToLoc apiPort apiManager apiKey) options
  cache <- newMVar (map (, []) listOfLocs, [])

  serverId <- forkIO $ run port (app apiPort apiManager apiKey cache timeError locError)

  requesterId <- forkIO $
    weatherRequester
      apiPort
      apiManager
      apiKey
      cache
      (fromIntegral utc)
      timeBetweenRequests

  cli

  killThread serverId
  killThread requesterId

  return ()


cli :: IO ()
cli = do
  inp <- getLine
  case inp of
    "stop" -> do
      putStrLn "stopping"
    _ -> do
      putStrLn "unknown command"
      cli
