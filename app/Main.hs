{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           Weather

-- http://api.openweathermap.org/geo/1.0/direct?q={City}&limit={num}&appid={API key}
-- https://api.openweathermap.org/data/2.5/weather?lat={lat}&lon={lon}&appid={API key}

apiKey = "d3893a4a4a6a642eadad642c112ed456" -- test api key hidden from git

second :: Int
second = 1_000_000

minute :: Int
minute = 60_000_000

hour :: Int
hour = 3_600_000_000

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM
    (cityRequest (Just "Moscow") (Just 1) (Just apiKey))
    (mkClientEnv manager' (BaseUrl Http "api.openweathermap.org" 80 ""))
  case res of
    Left err    -> putStrLn $ "Error: " ++ show err
    Right books -> print books

type APIKey = Text

type RequestGetCityLocation =
  "geo" :> "1.0" :> "direct"
  :> QueryParam "q" Text    -- City
  :> QueryParam "limit" Int -- Number of variants
  :> QueryParam "appid" APIKey
  :> Get '[JSON] Text

reqApi :: Proxy RequestGetCityLocation
reqApi = Proxy

cityRequest :: Maybe Text -> Maybe Int -> Maybe APIKey -> ClientM Text
cityRequest = client reqApi

type RequestWeatherLocation =
  "data" :> "2.5" :> "weather"
  :> QueryParam "lat" Double
  :> QueryParam "lon" Double
  :> QueryParam "appid" APIKey

