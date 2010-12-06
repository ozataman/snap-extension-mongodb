{-|


-}

module Snap.Extension.MongoDB.MongoDB
  ( MongoDBState
  , HasMongoDBState(..)
  , mongoDBInitializer
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import qualified Data.ByteString as B

import           Database.MongoDB
import           Snap.Extension
import           Snap.Types

import           Snap.Extension.MongoDB


------------------------------------------------------------------------------
-- | MongoDB State
data MongoDBState = MongoDBState
    { connPool :: ConnPool Host
    , appDatabase :: Database 
    }


------------------------------------------------------------------------------
-- |
class HasMongoDBState s where
    getMongoDBState :: s -> MongoDBState
    setMongoDBState :: MongoDBState -> s -> s

    modifyMongoDBState :: (MongoDBState -> MongoDBState) -> s -> s
    modifyMongoDBState f s = setMongoDBState (f $ getMongoDBState s) s


------------------------------------------------------------------------------
-- |
mongoDBInitializer :: Host
                   -> Int 
                   -> UString 
                   -> Initializer MongoDBState 
mongoDBInitializer h n db = do
  mongoState <- liftIO $ do
    pool <- newConnPool n h 
    return $ MongoDBState pool (Database db)
  mkInitializer mongoState


------------------------------------------------------------------------------
-- |
instance InitializerState MongoDBState where
  extensionId = const "MongoDB/MongoDB"
  mkCleanup s = killPipes $ connPool s
  mkReload = const $ return ()



------------------------------------------------------------------------------
-- |
instance HasMongoDBState s => MonadMongoDB (SnapExtend s) where
  withDB run = do
    (MongoDBState pool db) <- asks getMongoDBState
    access safe Master pool $ use db run
