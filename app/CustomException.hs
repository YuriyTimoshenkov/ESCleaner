module CustomException
( tryT
, tryReaderT
, YTBusinessException (YTBusinessException)
) where

import           Control.Exception.Base
import qualified Control.Monad.Logger       as LT
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           CustomLogging
import qualified Data.Text                  as T
import           Data.Typeable


data YTBusinessException = YTBusinessException {message :: T.Text} deriving (Show,Typeable)

instance Exception YTBusinessException

tryT :: IO a -> ExceptT SomeException (LT.LoggingT IO) a
tryT action = ExceptT $ lift $ try action

tryReaderT :: (Exception e) => IO a -> ExceptT e (ReaderT b (LT.LoggingT IO)) a
tryReaderT action = ExceptT $ lift . lift $ try action
