{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           City
import           Common
import           Control.Concurrent
import           Data.Either
import qualified Data.Map            as M
import qualified Data.Text           as T
import           Data.Weather
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           System.Environment
import           WeatherRequest
-- import qualified Options.Applicative as Opt


-- http://api.openweathermap.org/geo/1.0/direct?q={City}&limit={num}&appid={API key}
-- http://api.openweathermap.org/data/2.5/weather?lat={lat}&lon={lon}&appid={API key}

-- type Cache = MVar (M.Map Location (Vector FullWeatherDescription))

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

main :: IO ()
main = do
  apiKey <- T.pack <$> getEnv "WEATHERAPIKEY"
  -- inputList <- getArgs
  cities <- (fmap . fmap) T.pack getArgs
  apiManager <- newManager defaultManagerSettings
  inp <- traverse (getCityLocation 80 apiManager apiKey) cities
  let locs = inp
        |> filter isRight
        |> map (\(Right l) -> l)
  cws <- traverse (getCurrentWeather 80 apiManager apiKey) locs
  print cws

weatherRequester :: MVar () -> [Location] -> Int -> IO ()
weatherRequester cache locations time = do
  -- request here
  threadDelay time
  weatherRequester cache locations time


