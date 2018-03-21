{-# LANGUAGE TemplateHaskell #-}

import           Control.Exception.Base
import qualified Control.Monad.Logger       as LT
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import           CustomException
import           CustomLogging
import           ESIndexManagement
import           Threading

app :: LoggingRunner -> LT.LoggingT IO ()
app loggingRunner = do
  configuration <- return $ Configuration "https://search-royalscenic-phoenix-qz75srmbukdyf7nxwqj5rsocsm.ca-central-1.es.amazonaws.com/" 30
  result <- runExceptT $ runReaderT (removeOldIndexes loggingRunner) configuration
  case result of
    Left a  -> $(LT.logInfoSH) (a::SomeException)
    Right _ -> return ()


main = LT.runStdoutLoggingT $ app LT.runStdoutLoggingT
