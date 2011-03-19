module Snap.Extension.DB.MongoDB.Instances where


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


import Data.Bson
import Snap.Auth

import           Snap.Extension.DB.MongoDB.Utils


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
-- | Make conversion to-from UserId a bit easier
instance Val UserId where
    val (UserId bs) = val $ bs2objid bs 
    cast' x = fmap UserId . fmap objid2bs . cast' $ x



