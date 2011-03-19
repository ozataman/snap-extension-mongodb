module Snap.Extension.DB.MongoDB.Utils where

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

