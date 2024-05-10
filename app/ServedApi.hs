{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module ServedApi (app, Cache(..)) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text                as T
import           Data.Time.Clock
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics
import           GHC.OldList              (find)
import           Network.HTTP.Client      (Manager (..))
import           Servant
import           Servant.API

import           Data.Common
import           Data.Weather
import           OpenWeather.City

type Cache = (
  [(Location, [Maybe FullWeatherDescription])]
  , [UTCSeconds])

newtype ErrorExt
  = Err T.Text
  deriving (Eq, Generic)
instance ToJSON ErrorExt

zero :: UTCTime
Just zero = iso8601ParseM "1970-01-01T00:00:00Z" -- +00:00"

type ExposedApi =
  "weather" :> "location"
  :> Capture "lat" Float
  :> Capture "lon" Float
  :> Capture "time" T.Text
  :> Get '[JSON] (Either ErrorExt FullWeatherDescription)
  :<|>
  "weather" :> "city"
  :> Capture "name" T.Text
  :> Capture "time" T.Text
  :> Get '[JSON] (Either ErrorExt FullWeatherDescription)

server :: Int -> Manager -> APIKey -> MVar Cache -> UTCSeconds -> Float -> Server ExposedApi
server port man key cache timeErr locErr
  =    getByLoc
  :<|> getByName
  where

    getByLoc :: Float -> Float -> T.Text -> Handler (Either ErrorExt FullWeatherDescription)
    getByLoc lat lon time = do
      (cached, times) <- liftIO $ readMVar cache
      return $ do
        closeEnough <- find
              (\x -> dist (fst x) (Location lat lon) < locErr)
              cached
              <||> Err "No close enough locations"
        requestTime <- (iso8601ParseM $ T.unpack time :: Maybe UTCTime)
                       <||> Err "Incorrect time format"
        findByT times
          (snd closeEnough)
          (truncate (realToFrac $ diffUTCTime requestTime zero))
          timeErr
          <||> Err "No record"
          -- let record = findByT times records

    findByT :: [UTCSeconds] ->
               [Maybe FullWeatherDescription] ->
               UTCSeconds ->
               UTCSeconds ->
               Maybe FullWeatherDescription
    findByT listTimes listRecords time tErr = case listTimes of
      t : rest | t > time + tErr -> findByT rest (tail listRecords) time tErr
               | time - tErr <= t && time + tErr >= t -> head listRecords
      _ -> Nothing

    getByName :: T.Text -> T.Text ->  Handler (Either ErrorExt FullWeatherDescription)
    getByName city time = do
      locResp <- liftIO $ getCityLocation port man key city
      case locResp of
        Left err -> return (Left $ Err (T.pack ("error getting city location: " ++ show err)))
        Right (Location lat lon) -> getByLoc lat lon time


    (<||>) :: Maybe a -> b -> Either b a
    ma <||> b = case ma of
      Just a  -> Right a
      Nothing -> Left b

appApi :: Proxy ExposedApi
appApi = Proxy

app :: Int -> Manager -> APIKey -> MVar Cache -> UTCSeconds -> Float -> Application
app p m k cache timeErr locErr = serve appApi (server p m k cache timeErr locErr)




