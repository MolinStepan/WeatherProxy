{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           City
import           Common
import           Control.Concurrent
import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Options.Applicative as Opt
import           Servant.API
import           Servant.Client
import           System.Environment


-- http://api.openweathermap.org/geo/1.0/direct?q={City}&limit={num}&appid={API key}
-- http://api.openweathermap.org/data/2.5/weather?lat={lat}&lon={lon}&appid={API key}

type RequestWeatherLocation =
  "data" :> "2.5" :> "weather"
  :> QueryParam "lat" Double
  :> QueryParam "lon" Double
  :> QueryParam "appid" APIKey
  :> Get '[JSON] Text -- FIXME

main :: IO ()
main = do
  apiKey <- getEnv "WEATHERAPIKEY"
  -- inputList <- getArgs
  cities <- getArgs
  manager <- newManager defaultManagerSettings
  inp <- traverse ((getCityLocation 80 manager (pack apiKey)) . pack) cities
  putStrLn $ show inp


reqApi :: Proxy RequestWeatherLocation
reqApi = Proxy

weatherRequest :: Maybe Double -> Maybe Double -> Maybe APIKey -> ClientM Text
weatherRequest = client reqApi

weatherRequester :: MVar () -> [Location] -> Int -> IO ()
weatherRequester cache locations time = do
  -- request here
  threadDelay time
  weatherRequester cache locations time


