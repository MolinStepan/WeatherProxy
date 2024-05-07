module Common where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant.Client

type APIKey = Text

data Error'
  = CE ClientError
  | OE Text
  deriving (Eq, Show)

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
