module Snap.Extension.DB.MongoDB.Utils where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString (ByteString)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.CompactString.Internal as CSI
import qualified Data.CompactString.UTF8 as CS
import           Data.UString (u)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Word (Word8)
import           Data.Time

import           Database.MongoDB
import           Database.MongoDB as DB

import           Numeric (showHex, readHex)
import           Safe

import           Snap.Types
import           Snap.Auth
import           Snap.Extension


------------------------------------------------------------------------------
-- | Convert 'ObjectId' into 'ByteString'
objid2bs :: ObjectId -> ByteString
objid2bs (Oid a b) = B8.pack . showHex a . showChar '-' . showHex b $ ""


------------------------------------------------------------------------------
-- | Convert 'ByteString' into 'ObjectId'
bs2objid :: ByteString -> Maybe ObjectId
bs2objid bs = do
  case B8.split '-' bs of
    (a':b':_) -> do
      a <- fmap fst . headMay . readHex . B8.unpack $ a'
      b <- fmap fst . headMay . readHex . B8.unpack $ b'
      return $ Oid a b
    _ -> Nothing

------------------------------------------------------------------------------
-- | Like 'bs2objid', but may blow with an error if the 'ByteString' can't be
-- converted to an 'ObjectId'
bs2objid' :: ByteString -> ObjectId
bs2objid' = fromJust . bs2objid


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

