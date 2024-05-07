{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module City where

import           Common
import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client


type RequestGetCityLocation =
  "geo" :> "1.0" :> "direct"
  :> QueryParam "q" Text    -- City
  :> QueryParam "limit" Int -- Number of variants
  :> QueryParam "appid" APIKey
  :> Get '[JSON] [Location]

reqCityLocApi :: Proxy RequestGetCityLocation
reqCityLocApi = Proxy

cityRequest :: Maybe Text -> Maybe Int -> Maybe APIKey -> ClientM [Location]
cityRequest = client reqCityLocApi

getCityLocation :: Int -> Manager -> APIKey -> Text -> IO (Either Error' Location)
getCityLocation port manager key city = do
  res <- runClientM
    (cityRequest (Just city) (Just 1) (Just key))
    (mkClientEnv manager (BaseUrl Http "api.openweathermap.org" port ""))
  return $ case res of
    Right [loc] -> Right loc
    Right []    -> Left $ OE "No city"
    Left err    -> Left $ CE err
    _           -> Left $ OE "Wrong response" -- should never happen
