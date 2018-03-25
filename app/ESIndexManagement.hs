{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ESIndexManagement
( removeOldIndexes
, Configuration (Configuration)
) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Logger       as LT
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy       as BL
import           Data.Either
import           Data.List
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import           CustomException
import           CustomLogging
import           Threading
import           Control.Concurrent.Async


data ELKIndex = ELKIndex { name :: T.Text, date :: Day } deriving (Show)
data Configuration = Configuration { elkUrl :: String, minAgeOfIndexInDays :: Integer }


parseIndexNames :: [T.Text] -> [Either YTBusinessException ELKIndex]
parseIndexNames indexList =
  let getDate fileName =
        let date = (parseTimeM True defaultTimeLocale "%Y.%-m.%-d" (T.unpack $ T.takeWhileEnd (/= '-') fileName)) :: Maybe Day
        in case date of
          Just d  -> Right d
          Nothing -> Left $ YTBusinessException $ (T.pack "Date parsing failed for file name = ") <> fileName
      parseIndex index =
        let splittedIndex = T.words index
        in if length splittedIndex < 3
            then Left $ YTBusinessException $ (T.pack ("Index array too short -  " ++ (show index)))
            else
              case (getDate $ splittedIndex !! 2) of
                    Right d  -> Right $ ELKIndex (splittedIndex !! 2) d
                    Left err -> Left err
  in fmap parseIndex indexList


filterIndexesOlderThenMonth :: [ELKIndex] -> Integer -> UTCTime -> [ELKIndex]
filterIndexesOlderThenMonth index minAgeOfIndexInDays currentDateTime =
  let doesDateFit date = if diffDays (utctDay currentDateTime) date > minAgeOfIndexInDays
                            then True
                            else False
  in filter (doesDateFit . date) index


loadIndexListMock :: IO [T.Text]
loadIndexListMock =
  return ["yellow open   phoenix_release-2018.03.16 7K2o3gV3SsqtwgaWCJGk2g   5   1      91996            0      2.2gb          2.2gb",
   "yellow open   phoenix_release-2018.02.15 J1ZoSiOWQzCpOxVbU8X9-A   5   1      55631            0    326.5mb        326.5mb"]


loadIndexListWeb :: String -> IO [T.Text]
loadIndexListWeb url = do
  elsIndexResponse <- simpleHttp $ url ++ "_cat/indices?v&s=store.size:desc"
  decodedResponse <- return $ decodeUtf8 . BL.toStrict $ elsIndexResponse
  return $ tail $ T.splitOn "\n" decodedResponse


deleteIndex :: String -> ELKIndex -> LT.LoggingT IO ThreadId
deleteIndex url index = do
  $(LT.logInfoSH) $ "Before index removal " ++ (show index)
  initReq <- parseRequest $ url ++ T.unpack (name index)
  let request = initReq
            { method = "DELETE"
            }
  manager <- lift $ newManager tlsManagerSettings
  response <- httpLbs request manager
  if responseStatus response /= status200
    then throw $ YTBusinessException $ T.pack ("Error deleting index " ++ (show index) ++ ". Status code = " ++ (show $ responseStatus response))
    else return ()
  $(LT.logInfoSH) $ "Response statues: " ++ (show $ responseStatus response)
  $(LT.logInfoSH) $ "Index " ++ (T.unpack $ name index) ++ " deleted"
  lift myThreadId


removeOldIndexes :: LoggingRunner -> ReaderT Configuration (ExceptT SomeException (LT.LoggingT IO)) ()
removeOldIndexes loggingRunner = do
  Configuration{elkUrl=elkUrl, minAgeOfIndexInDays=minAgeOfIndexInDays} <- ask
  lift $ do
    indexListResp <- tryT $ loadIndexListWeb elkUrl
    indexList <- return $ parseIndexNames indexListResp
    $(LT.logInfoSH) $ "Parsed indexes : " <> show indexList

    indexList <- return $ rights $ filter isRight indexList
    $(LT.logInfoSH) $ "Loaded indexes: " <> show indexList

    currentDateTime <- liftIO getCurrentTime
    indexList <- return $ filterIndexesOlderThenMonth indexList minAgeOfIndexInDays currentDateTime
    $(LT.logInfoSH) $ "Filtered indexes: " <> show indexList

    threadAwaiters <- liftIO $ mapM (run . loggingRunner . deleteIndex elkUrl :: ELKIndex -> IO (AsyncResult (Async ThreadId))) indexList
    finishedThreadIds <- liftIO $ mapM (\awaiter -> Threading.wait awaiter) threadAwaiters

    $(LT.logInfoSH) $ "Finished threads: " <> show finishedThreadIds

    LT.logInfoN "App finished"
