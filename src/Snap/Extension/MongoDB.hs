{-|

'Snap.Extension.MongoDB' enables simple access to MongoDB databases inside of
your Snap applications. There is currently a single back-end and therefore this
library is presented in a single module.

For convenience, this module will also export the 'Database.MongoDB' module,
which means you don't have to import anything else.

To get started, make your 'ApplicationState' an instance of 'HasMongoDBState'
and use 'mongoDBInitializer' in your application's initializer. Your
'Application' will then be a 'MonadMongoDB'.

-}

module Snap.Extension.MongoDB
  ( 
    -- * MongoDB Functionality Inside Snap Monad 
    MonadMongoDB(..)

    -- * Implementation
    
    -- ** Keeping MongoDB State
  , MongoDBState(..)
  , HasMongoDBState(..)

    -- ** Initializing Your Applications
  , mongoDBInitializer

    -- * MongoDB Library 
    -- | Exported for your convenience.
  , module Database.MongoDB
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Word (Word8)

import           Database.MongoDB

import           Snap.Types
import           Snap.Extension


------------------------------------------------------------------------------
-- | The 'MonadMongoDB' class. Minimal complete definition:
class MonadSnap m => MonadMongoDB m where

  ----------------------------------------------------------------------------
  -- | Run given MongoDB action against the database
  withDB :: ReaderT Database (Action IO) a -> m (Either Failure a)


  ----------------------------------------------------------------------------
  -- | Same as 'withDB' but calls 'error' if there is an exception
  withDB' :: ReaderT Database (Action IO) a -> m a
  withDB' run = do
    r <- withDB run 
    either (error . show) return r



------------------------------------------------------------------------------
-- | Get strict ByteString to work directly with BSON auto-casting
instance Val B8.ByteString where
    val = val . B8.unpack
    cast' x = fmap B8.pack . cast' $ x


------------------------------------------------------------------------------
-- | Get [Octet] to work directly with BSON auto-casting
instance Val [Word8] where
    val = val . fmap w2c
    cast' x = fmap (fmap c2w) . cast' $ x


------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Implementation
------------------------------------------------------------------------------
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
    pool <- newConnPool Internet n h
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
    liftIO . access safe Master pool $ use db run
