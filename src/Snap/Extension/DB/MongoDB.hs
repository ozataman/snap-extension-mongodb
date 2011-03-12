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

module Snap.Extension.DB.MongoDB
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
  , lp

    -- * Snap.Auth Interface
    -- $monadauth
  , docToAuthUser
  , authUserToDoc

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
import qualified Data.Text as T
import qualified Data.CompactString.Internal as CSI
import qualified Data.CompactString.UTF8 as CS
import           Data.UString (u)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Word (Word8)
import           Data.Time

import           Numeric (showHex, readHex)
import           Safe

import           Database.MongoDB
import           Database.MongoDB as DB

import           Snap.Types
import           Snap.Auth
import           Snap.Extension


-- $monadauth
-- This package gives you free MonadAuthUser instances of your application
-- monad. Once your application becomes MonadMongoDB, if it is also MonadAuth,
-- it will automatically become MonadAuthUser.
--
-- This means you can immediately start using authentication functionality
-- without worrying about schema, fields, etc. This library will take care of
-- that for you.

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
-- | Get strict 'ByteString' to work directly with BSON auto-casting
instance Val B8.ByteString where
    val = val . B8.unpack
    cast' x = fmap B8.pack . cast' $ x


------------------------------------------------------------------------------
-- | Get strict 'Text' to work directly with BSON auto-casting
instance Val T.Text where
    val = val . T.unpack
    cast' x = fmap T.pack . cast' $ x


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
    cast' d@(Doc _) = fmap (Map.fromList . map convert . Map.toList) csiCast
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
-- | Add timestamps to any document.
addTimeStamps :: (MonadMongoDB m) => Document -> m Document
addTimeStamps d = do
  t <- liftIO getCurrentTime
  let tsc = ["created_at" =: t]
  let tsu = ["updated_at" =: t]
  return $ tsu `DB.merge` d `DB.merge` tsc


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


bs2cs :: ByteString -> UString
bs2cs = CSI.CS


------------------------------------------------------------------------------
-- | If the 'Document' has an 'ObjectId' in the given field, return it as
-- 'ByteString'
getObjId :: UString -> Document -> Maybe ByteString
getObjId v d = Database.MongoDB.lookup v d >>= fmap objid2bs


-- | Easy lookup from Snap's 'Params'
lp :: ByteString -> Params -> Maybe ByteString
lp n m = Map.lookup n m >>= headMay



------------------------------------------------------------------------------
-- Snap Auth Interface
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Make conversion to-from UserId a bit easier
instance Val UserId where
    val (UserId bs) = val $ bs2objid bs 
    cast' x = fmap UserId . fmap objid2bs . cast' $ x


------------------------------------------------------------------------------
-- | Turn a page from the database into 'AuthUser'
docToAuthUser :: Document -> Maybe AuthUser
docToAuthUser v = do
  uid <- DB.lookup "_id" v
  pass <- DB.lookup "password" v
  salt <- DB.lookup "salt" v
  return emptyAuthUser
            { userId = Just uid 
            , userEmail = DB.lookup "email" v
            , userPassword = Just $ Encrypted pass 
            , userSalt = Just salt
            , userActivatedAt = DB.lookup "activated_at" v
            , userSuspendedAt = DB.lookup "suspended_at" v
            , userPersistenceToken = DB.lookup "persistence_token" v
            , userCreatedAt = DB.lookup "created_at" v
            , userUpdatedAt = DB.lookup "updated_at" v
            , userCurrentLoginAt = DB.lookup "current_login_at" v
            , userLastLoginAt = DB.lookup "last_login_at" v
            , userCurrentLoginIp = DB.lookup "current_login_ip" v
            , userLastLoginIp = DB.lookup "last_login_ip" v
            , userLoginCount = maybe 0 id $ DB.lookup "login_count" v
            , userFailedLoginCount = maybe 0 id $ DB.lookup "failed_login_count" v
            }


------------------------------------------------------------------------------
-- | Turn an 'AuthUser' into a 'Document' ready to be commited to DB.
authUserToDoc :: AuthUser -> Document
authUserToDoc usr = fields'
  where
    fields' = foldr step [] fields
    step x acc = maybe acc (: acc) x
    decidePass (Encrypted x) = Just ("password" =: x)
    decidePass _ = error "Can't save user without a proper password set"
    fields = 
      [ userId usr >>= return . ("_id" =:)    -- only if present
      , userCreatedAt usr >>= return . ("created_at" =:)  -- only if present
      , Just $ ("email" =: userEmail usr)
      , userPassword usr >>= decidePass
      , Just $ ("salt" =: userSalt usr)
      , Just $ ("activated_at" =: userActivatedAt usr)
      , Just $ ("suspended_at" =: userSuspendedAt usr)
      , Just $ ("persistence_token" =: userPersistenceToken usr)
      , Just $ ("current_login_at" =: userCurrentLoginAt usr)
      , Just $ ("last_login_at" =: userLastLoginAt usr)
      , Just $ ("current_login_ip" =: userCurrentLoginIp usr)
      , Just $ ("last_login_ip" =: userLastLoginIp usr)
      , Just $ ("login_count" =: userLoginCount usr)
      , Just $ ("failed_login_count" =: userFailedLoginCount usr)
      ]


instance (MonadAuth m, MonadMongoDB m) => MonadAuthUser m Document where

  getUserInternal uid = do
    t' <- fmap u authUserTable
    r <- withDB' $ findOne (select ["_id" =: uid] t')
    return $ do
      d <- r 
      (,) <$> docToAuthUser d <*> r


  getUserByRememberToken t = do
    t' <- fmap u authUserTable
    r <- withDB' $ findOne (select ["persistence_token" =: t] t')
    return $ do
      d <- r
      (,) <$> docToAuthUser d <*> r


  getUserExternal (EUId ps) = do
    lookup_keys <- authAuthenticationKeys
    t' <- fmap u authUserTable
    r <- withDB' $ findOne (select (buildConditions lookup_keys) t')
    return $ do
      d <- r 
      (,) <$> docToAuthUser d <*> r
    where 
      buildConditions ks = map cond ks
        where cond k = bs2cs k =: (fmap bs2cs $ lp k ps)


  saveAuthUser (user, d0) = do
    t' <- fmap u authUserTable
    user' <- updateUser
    d <- addTimeStamps $ authUserToDoc user'
    let d' = d `DB.merge` d0
    case userId user of
      Just _ -> do    -- Existing user
        withDB' $ save t' d' >> return Nothing
        return . Just $ user'
      Nothing -> do   -- New user
        uid <- withDB' $ insert t' d' 
        return . Just $ user' { userId = cast' uid }
    where
      updateUser = case userPassword user of
        Just (ClearText x) -> updateUserPass x
        Nothing -> error "Can't save user without any form of password"
        _ -> return user
      updateUserPass x = do
        (newsalt, newpass) <- mkAuthCredentials x
        return $ user { userPassword = Just (Encrypted newpass)
                      , userSalt = Just newsalt }


