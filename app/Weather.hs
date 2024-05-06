module Weather where

data Weather = Weather

data Wind
  = Wind
      { speed :: Double
      , deg   :: Double
      , gust  :: Double
      }

data Main
  = Main
      { temp      :: Double
      , feelsLike :: Double
      , tempMin   :: Double
      , tempMax   :: Double
      , pressure  :: Int
      , humidity  :: Int
      , seaLevel  :: Int
      , grndLevel :: Int
      }


