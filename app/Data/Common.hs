module Data.Common (readTime, dist, UTCSeconds, Error' (..), APIKey, Location (..)) where

import           Data.Aeson
import           Data.Char
import qualified Data.Text      as T
import           GHC.Generics
import           Servant.Client

type APIKey = T.Text
type UTCSeconds = Int

data Error'
  = CE ClientError
  | OE T.Text
  deriving (Eq, Show)

data Location
  = Location
      { lat :: Float
      , lon :: Float
      }
  deriving (Eq, Generic, Show)
instance FromJSON Location
instance ToJSON Location

readTime :: String -> Maybe UTCSeconds
readTime str = case str of
  [a,b,':',c,d] | foldr (&&) True (map isDigit [a,b,c,d]) -> let
                    [a',b',c',d'] = map digitToInt [a,b,c,d]
                    in Just $ (10*a'+b')*3600 + (10*c'+d')*60
  [a,b,':',c,d,':',e,f] | foldr (&&) True (map isDigit [a,b,c,d,e,f])-> let
                            [a',b',c',d',e',f'] = map digitToInt [a,b,c,d,e,f]
                            in Just $ (10*a'+b')*3600 + (10*c'+d')*60 + (10*e'+f')
  _ -> Nothing

dist :: Location -> Location -> Float
dist (Location theta1' lambda1') (Location theta2' lambda2') = let
  dLambda' = abs (lambda1' - lambda2')
  dLambda = (if dLambda' >= 360 then dLambda' - 360 else dLambda') * pi / 180
  theta1 = theta1' * pi / 180
  theta2 = theta2' * pi / 180
  in
  atan $ (sqrt (((cos theta1)*(sin dLambda))^2 +
                  ((cos theta1)*(sin theta2)-(sin theta1)*(cos theta2)*(cos dLambda))^2)
          ) / ((sin theta1)*(sin theta2) + (cos theta1)*(cos theta2)*(cos dLambda))
