module ArgumentsParser (InputPoint (..), readComLineArgs) where

import           Data.Common
import qualified Data.Text           as T
import           Options.Applicative
import           Text.Read

data InputPoint
  = City T.Text
  | Loc Location
  deriving (Eq, Show)

readComLineArgs :: IO [InputPoint]
readComLineArgs = execParser opts

reader :: ReadM InputPoint
reader = maybeReader inpPointM

inpPointM :: String -> Maybe InputPoint
inpPointM s = let
  loc = do
    (lat,lon) <- readMaybe s
    return $ Loc (Location lat lon)
  cit = return $ City (T.pack s)
  in loc <|> cit


options :: Parser [InputPoint]
options = many $ argument reader (metavar "TARGET...")

opts :: ParserInfo [InputPoint]
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

