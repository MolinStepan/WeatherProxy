{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           System.Environment
import           Weather

-- http://api.openweathermap.org/geo/1.0/direct?q={City}&limit={num}&appid={API key}
-- http://api.openweathermap.org/data/2.5/weather?lat={lat}&lon={lon}&appid={API key}

type APIKey = Text

type RequestGetCityLocation =
  "geo" :> "1.0" :> "direct"
  :> QueryParam "q" Text    -- City
  :> QueryParam "limit" Int -- Number of variants
  :> QueryParam "appid" APIKey
  :> Get '[JSON] Text

type RequestWeatherLocation =
  "data" :> "2.5" :> "weather"
  :> QueryParam "lat" Double
  :> QueryParam "lon" Double
  :> QueryParam "appid" APIKey
  :> Get '[JSON] Testing

data Testing
  = Testing
      { timezone :: Int
      , id       :: Int
      , name     :: String
      }
  deriving (Eq, Generic, Show)
instance FromJSON Testing

data Location
  = Location
      { lat :: Float
      , lon :: Float
      }
  deriving (Eq, Generic, Show)
instance FromJSON Location

second :: Int
second = 1_000_000

minute :: Int
minute = 60_000_000

hour :: Int
hour = 3_600_000_000

main :: IO ()
main = do
  apiKey <- getEnv "WEATHERAPIKEY"
  -- inputList <- getArgs
  [portS] <- getArgs
  manager' <- newManager defaultManagerSettings
  res <- runClientM
    (weatherRequest (Just 55.75) (Just 37.61) (Just $ pack apiKey))
    (mkClientEnv manager' (BaseUrl Http "api.openweathermap.org" (read portS) ""))
  case res of
    Left err    -> putStrLn $ "Error: " ++ show err
    Right books -> print books

reqCityLocApi :: Proxy RequestGetCityLocation
reqCityLocApi = Proxy

cityRequest :: Maybe Text -> Maybe Int -> Maybe APIKey -> ClientM Text
cityRequest = client reqCityLocApi


reqApi :: Proxy RequestWeatherLocation
reqApi = Proxy

weatherRequest :: Maybe Double -> Maybe Double -> Maybe APIKey -> ClientM Testing
weatherRequest = client reqApi

weatherRequester :: MVar () -> [Location] -> Int -> IO ()
weatherRequester cache locations time = do
  -- request here
  threadDelay time
  weatherRequester cache locations time


