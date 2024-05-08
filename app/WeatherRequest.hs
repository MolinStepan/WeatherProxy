{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module WeatherRequest where

import           Common
import           Data.Proxy
import           Data.Text
import           Data.Weather
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client

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

getCurrentWeather :: Int -> Manager -> APIKey -> Location -> IO (Either Error' FullWeatherDescription)
getCurrentWeather port manager key (Location lat lon) = do
  res <- runClientM
    (weatherRequest (Just lat) (Just lon) (Just key))
    (mkClientEnv manager (BaseUrl Http "api.openweathermap.org" port ""))
  return $ case res of
    Right fwd -> Right fwd
    Left err  -> Left $ CE err
