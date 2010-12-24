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


    -- * Utility Functions
  , getObjId
  , bs2objid
  , objid2bs

    -- * MongoDB Library 
    -- | Exported for your convenience.
  , module Database.MongoDB
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString (ByteString)
import qualified Data.CompactString.Internal as CSI
import qualified Data.CompactString.UTF8 as CS
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Word (Word8)

import           Numeric (showHex, readHex)

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
-- | Make Map UString b an instance of Val for easy conversion of values
instance (Val b) => Val (Map UString b) where
    val m = val doc
      where f (k,v) = k =: v
            doc = map f $ Map.toList m
    cast' (Doc x) = Map.fromList <$> mapM separate x 
      where separate ((:=) k v) = (,) <$> (return k) <*> (cast' v)
    cast' _ = Nothing


------------------------------------------------------------------------------
-- | Make Map ByteString b an instance of Val for easy conversion of values
instance (Val b) => Val (Map ByteString b) where
    val = val . Map.fromList . map convert . Map.toList 
      where convert (k,v) = (bs2cs k, v)
            bs2cs :: ByteString -> UString
            bs2cs = CSI.CS
    cast' d@(Doc x) = fmap (Map.fromList . map convert . Map.toList) csiCast
      where convert ((CSI.CS k), v) = (k, v)
            csiCast :: (Val c) => Maybe (Map UString c)
            csiCast = cast' d 
    cast' _ = Nothing
            

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




------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Convenience Functions
------------------------------------------------------------------------------
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Convert 'ObjectId' into 'ByteString'
objid2bs :: ObjectId -> ByteString
objid2bs (Oid a b) = B8.pack . showHex a . showChar '-' . showHex b $ ""


------------------------------------------------------------------------------
-- | Convert 'ByteString' into 'ObjectId'
bs2objid :: ByteString -> ObjectId
bs2objid bs = Oid a b
  where (a',b') = break (== '-') . B8.unpack $ bs
        a = fst . head . readHex $ a'
        b = fst . head . readHex $ drop 1 b'


------------------------------------------------------------------------------
-- | If the 'Document' has an 'ObjectId' in the given field, return it as
-- 'ByteString'
getObjId :: UString -> Document -> Maybe ByteString
getObjId v d = Database.MongoDB.lookup v d >>= fmap objid2bs



