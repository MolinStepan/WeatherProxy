{-# LANGUAGE OverloadedStrings #-}

module Data.Weather where

import           Common
import           Data.Aeson
import           Data.Text
import           GHC.Generics

data FullWeatherDescription
  = FWD
      { coord      :: Location
      , weather    :: [Weather]
      , base       :: Text
      , main'      :: Main
      , visibility :: Int
      , wind       :: Wind
      , clouds     :: Clouds
      , rain       :: Maybe MMs
      , snow       :: Maybe MMs
      }
  deriving (Eq, Generic, Show)
instance FromJSON FullWeatherDescription where
  parseJSON = withObject "FullWeatherDescripton" $ \v -> FWD
    <$> v .: "coord"
    <*> v .: "weather"
    <*> v .: "base"
    <*> v .: "main"
    <*> v .: "visibility"
    <*> v .: "wind"
    <*> v .: "clouds"
    <*> v .:? "rain"
    <*> v .:? "snow"

data Weather
  = Weather
      { id          :: Int
      , mainWeather :: Text
      , descrpition :: Text
      , icon        :: Text
      }
  deriving (Eq, Generic, Show)
instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v -> Weather
    <$> v .: "id"
    <*> v .: "main"
    <*> v .: "description"
    <*> v .: "icon"

data Wind
  = Wind
      { speed :: Float
      , deg   :: Float
      , gust  :: Maybe Float
      }
  deriving (Eq, Generic, Show)
instance FromJSON Wind

data Main
  = Main
      { temp      :: Float
      , feelsLike :: Float
      , pressure  :: Int
      , humidity  :: Int
      , tempMin   :: Float
      , tempMax   :: Float
      , seaLevel  :: Maybe Int
      , grndLevel :: Maybe Int
      }
  deriving (Eq, Generic, Show)
instance FromJSON Main where
  parseJSON = withObject "Main" $ \v -> Main
    <$> v .: "temp"
    <*> v .: "feels_like"
    <*> v .: "pressure"
    <*> v .: "humidity"
    <*> v .: "temp_min"
    <*> v .: "temp_max"
    <*> v .:? "sea_level"
    <*> v .:? "grnd_level"

newtype Clouds
  = Clouds { all :: Int }
  deriving (Eq, Generic, Show)
instance FromJSON Clouds

data MMs
  = MMs
      { oneHour    :: Maybe Float
      , threeHours :: Maybe Float
      }
  deriving (Eq, Generic, Show)
instance FromJSON MMs where
  parseJSON = withObject "MMs" $ \v -> MMs
    <$> v .:? "1h"
    <*> v .:? "3h"
