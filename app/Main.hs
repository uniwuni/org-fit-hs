{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import System.Environment (getArgs)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Semigroup ((<>))
import Network.Google.OAuth2
import Network.HTTP.Simple
import Network.OAuth.OAuth2
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Text.Read
import Data.Text (pack)
import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
--import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock.POSIX(posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
data Activity = Activity { startTime :: UTCTime, endTime :: UTCTime, activityType :: Int } deriving Show
newtype Activities = Activities { activities :: [Activity] } deriving (Show)

brokenDecimal = fromInteger . fst . either undefined id . decimal

instance FromJSON Activities where
  parseJSON = withObject "Activities" $ \obj -> do
    obj .: "point" >>=
      withArray "Points" (\arr ->
        do
          let x = sequence $ fmap parseActivity arr

          Activities <$> V.toList <$> x)

    where parseActivity = withObject "activity" $
            \act -> do
              start <- parseTime =<< act .: "startTimeNanos"
              end   <- parseTime =<< act .: "endTimeNanos"
              acttype <- parseType =<< (act .: "value")
              return $ Activity start end acttype

          parseType :: Value -> Parser Int
          parseType = withArray "value" $ \arr -> withObject "value" (\act -> act .: "intVal") (arr V.! 0)

          parseTime :: Value -> Parser UTCTime
          parseTime = withText "Time" (\t -> return $ epochToUTC $ ((brokenDecimal t :: Int) `div` 1000000000))

timeToOrgMode :: UTCTime -> IO String
timeToOrgMode time = do
  timeZone <- getCurrentTimeZone
  let b = utcToLocalTime timeZone time
  return $ "[" ++ init (init (init (show b))) ++ "]"

activityToOrgMode :: Activity -> IO String
activityToOrgMode a = do
  time1 <- timeToOrgMode $ startTime a
  time2 <- timeToOrgMode $ endTime a
  if activityType a == 0 || diffUTCTime (endTime a) (startTime a) < 180
    then return "" else return $ "CLOCK: " ++ time1 ++ "--" ++ time2 ++ "\n"

epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

utcToNanos :: UTCTime -> String
utcToNanos time = show $ (1000000000*) $ ceiling $ toRational  $ utcTimeToPOSIXSeconds time

dayToNanos :: (Integer, Int, Int) -> (Integer, Integer, Integer) -> String
dayToNanos (y,m,d) (h,mn, s) = utcToNanos (UTCTime (fromGregorian y m d) $ secondsToDiffTime (h * 3600 + mn * 60 + s))

-- CLOCK: [2022-10-01 Sa 14:26]--[2022-10-01 Sa 14:47] =>  0:21

correctFitnessValues :: String -> IO (Maybe Activities)
correctFitnessValues x= do
  t <- getCurrentTime
  fitnessValues (read x) t

fitnessValues :: UTCTime -> UTCTime -> IO (Maybe Activities)
fitnessValues from to = do
    home <- getHomeDirectory
    [id, secret] <- words <$> readFile (home </> ".local/share/org-fit-hs.credentials")
    OAuth2Token{..} <-
      getAccessToken
        (pack id)      -- Fill with real ID.
        (pack secret)  -- Fill with real code.
        ["https://www.googleapis.com/auth/fitness.activity.read"]
        (Just "credentials.cache")

    request <- parseRequest $ "https://www.googleapis.com/fitness/v1/users/me/dataSources/derived:com.google.activity.segment:com.google.android.gms:merge_activity_segments/datasets/"++ utcToNanos from++"-"++ utcToNanos to
    response <- httpJSON $ authorize (atoken accessToken) request

    let result = (fromJSON (getResponseBody response) :: Result Activities)
    case result of
      Success b -> pure $ Just b
      Error x -> print x >> pure Nothing
 where
   authorize token = setRequestHeaders
       [ ("Authorization", encodeUtf8 $ "Bearer " <> token)
       ]


main = do
  home <- getHomeDirectory
  setCurrentDirectory (home </> ".local/bin")
  timeStr <- head <$> getArgs
  values' <- correctFitnessValues timeStr
  let data1' = (fmap activityToOrgMode . activities) <$> values'
  data2 <- maybe (return []) id (fmap sequence data1')
  mapM_ putStr data2

