{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenWeather.WeatherRequest where

import           Control.Concurrent
import           Data.Either
import           Data.Proxy
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Time.Format.ISO8601
import           Network.HTTP.Client      (Manager)
import           Servant.API
import           Servant.Client

import           Data.Common
import           Data.Weather
import           ServedApi

type RequestWeatherLocation =
  "data" :> "2.5" :> "weather"
  :> QueryParam "lat" Float
  :> QueryParam "lon" Float
  :> QueryParam "appid" APIKey
  :> Get '[JSON] FullWeatherDescription -- FIXME

reqApi :: Proxy RequestWeatherLocation
reqApi = Proxy

weatherRequest :: Maybe Float -> Maybe Float -> Maybe APIKey -> ClientM FullWeatherDescription
weatherRequest = client reqApi

getCurrentWeatherEither :: Int -> Manager -> APIKey -> Location -> IO (Either Error' FullWeatherDescription)
getCurrentWeatherEither port manager key (Location lat lon) = do
  res <- runClientM
    (weatherRequest (Just lat) (Just lon) (Just key))
    (mkClientEnv manager (BaseUrl Http "api.openweathermap.org" port ""))
  return $ case res of
    Right fwd -> Right fwd
    Left err  -> Left $ CE err


weatherRequester ::
  Int ->
  Manager ->
  APIKey ->
  MVar Cache ->
  UTCSeconds ->
  UTCSeconds ->
  IO ()
weatherRequester p m key cache time delay = do
  (oldCache, times) <- readMVar cache
  retLoc <- newEmptyMVar
  updateCache (map fst oldCache) retLoc
  newRecords <- takeMVar retLoc

  -- Log errors
  let newRecords' = map eToM newRecords

  let !updatedCache =
        ( zipWith (\(loc, weaths) weath -> (loc, weath : weaths)) oldCache newRecords'
        , fromIntegral time : times
        )
  _ <- takeMVar cache
  putMVar cache updatedCache

  -- logging
  a <- getCurrentTime
  putStrLn $ iso8601Show a ++ ": added records"

  MkSystemTime currentTime _ <- getSystemTime
  threadDelay $ (time + delay - fromIntegral currentTime) * 1_000_000
  weatherRequester p m key cache (time + delay) delay
  where
    updateCache ::
      [Location] ->
      MVar [Either Error' FullWeatherDescription] ->
      IO ()
    updateCache list putHere = case list of
      [] -> do
        putMVar putHere []
      x : rest -> do
        returnLoc <- newEmptyMVar
        updateCache rest returnLoc
        res <- getCurrentWeatherEither p m key x
        rest' <- takeMVar returnLoc
        putMVar putHere (res : rest')

    eToM e = case e of
      Right x -> Just x
      Left _  -> Nothing

