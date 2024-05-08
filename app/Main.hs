{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           City
import           Common
import           Control.Concurrent
import           Data.Either
import qualified Data.Text              as T
import           Data.Time.Clock.System
import           Data.Weather
import           Network.HTTP.Client    (Manager (..), defaultManagerSettings,
                                         newManager)
import qualified Options.Applicative    as Opt
import           System.Environment
import           WeatherRequest

-- http://api.openweathermap.org/geo/1.0/direct?q={City}&limit={num}&appid={API key}
-- http://api.openweathermap.org/data/2.5/weather?lat={lat}&lon={lon}&appid={API key}

type Cache = (
  [(Location, [Maybe FullWeatherDescription])]
  , [UTCSeconds])

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

type UTCSeconds = Int

main :: IO ()
main = do
  apiKey <- T.pack <$> getEnv "WEATHERAPIKEY"
  timeBetweenRequests <- read <$> getEnv "WEATHERPROXYINTERVAL"
  port <- read <$> getEnv "WEATHERPROXYPORT"
  inputList <- getArgs
  apiManager <- newManager defaultManagerSettings
  MkSystemTime utc _ <- getSystemTime
  cache <- newMVar ([(Location 55 37, []), (Location 55 82, [])], [])

  forkIO $ weatherRequester port apiManager apiKey cache (fromIntegral utc) timeBetweenRequests

  threadDelay 240_000_000

  return ()

weatherRequester :: Int -> Manager -> APIKey -> MVar Cache -> UTCSeconds -> UTCSeconds -> IO ()
weatherRequester p m key cache time delay = do
  -- request here
  (oldCache, times) <- readMVar cache
  retLoc <- newEmptyMVar
  updateCache p m key (map fst oldCache) retLoc

  newRecords <- takeMVar retLoc

  traverse print (filter isLeft newRecords)
  let newRecords' = map eToM newRecords

  let !updatedCache =
        ( zipWith (\(loc, weaths) weath -> (loc, weath : weaths)) oldCache newRecords'
        , fromIntegral time : times
        )
  _ <- takeMVar cache
  putMVar cache updatedCache

  MkSystemTime currentTime _ <- getSystemTime
  threadDelay $ (time + delay - fromIntegral currentTime) * 1_000_000
  weatherRequester p m key cache (time + delay) delay
  where
    updateCache :: Int -> Manager -> APIKey -> [Location] -> MVar [Either Error' FullWeatherDescription] -> IO ()
    updateCache p m key list putHere = case list of
      [] -> do
        putMVar putHere []
      x : rest -> do
        returnLoc <- newEmptyMVar
        updateCache p m key rest returnLoc
        res <- getCurrentWeatherEither p m key x
        rest' <- takeMVar returnLoc
        putMVar putHere (res : rest')


    eToM e = case e of
      Right x -> Just x
      Left _  -> Nothing




