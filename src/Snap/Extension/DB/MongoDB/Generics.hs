{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Snap.Extension.DB.MongoDB.Generics 

(
  -- * Conversion Functions
  toDoc
, fromDoc

  -- * Useful Types
, RecKey(..)
, Optional(..)

  -- * Needed typeclasses for Generics support
, ToDoc(..)
, FromDoc(..)

  -- * Regular generics library exported for convenience
, module Generics.Regular

)

where

import Control.Applicative
import Control.Monad

import Data.Bson
import Data.Typeable
import Data.Monoid hiding (Product)
import qualified Data.Bson as D
import Data.UString (u)
import qualified Data.Typeable as T
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import qualified Data.CompactString.Internal as CSI
import qualified Data.Map as Map
import           Data.Word (Word8)

import Generics.Regular

import Snap.Extension.DB.MongoDB.Instances
import Snap.Extension.DB.MongoDB.Utils


------------------------------------------------------------------------------
-- | Use 'RecKey' type to map the _id attribute to your ADT.
--
-- This field will get treated differently. If it is there, it will be used. If
-- you put a 'Nothing', it will be ommitted so that MongoDB assigns one
-- automatically. Helpful when a record is being created.
newtype RecKey = RecKey { unRK :: Maybe ObjectId }
  deriving (Eq, Show, Typeable)


------------------------------------------------------------------------------
-- | Datatype to encode optional arguments.
--
-- Sometimes, we don't want Document to data-type conversion to fail just
-- because some field is not present in the database. Define such fields as
-- 'Optional' and they will get special treatment in cases where they are
-- missing entirely.
newtype Optional a = Optional { unOpt :: Maybe a }
  deriving (Eq, Show, Monad, Functor, Applicative, MonadPlus, Monoid, Typeable)


------------------------------------------------------------------------------
-- | 'Val' instance for 'Optional'
instance (Val a) => Val (Optional a) where
    val (Optional x) = val x
    cast' x = fmap Optional $ cast' x 

------------------------------------------------------------------------------
-- Generics typeclass to convert ADTs to 'Document'
--
class ToDoc f where
  toDocPF :: f a -> Document

instance ToDoc U where
  toDocPF _ = []

instance ToDoc I where
  toDocPF (I r) = []

instance (Regular a, ToDoc (PF a)) => ToDoc (K a) where
  toDocPF (K r) = toDoc r
  toDocPF _ = []

instance (Selector s, Val r) => ToDoc (S s (K r)) where
  toDocPF s@(S (K x)) = [u (selName s) =: x]
  toDocPF _ = []

instance (Selector s) => ToDoc (S s (K RecKey)) where
  toDocPF s@(S (K (RecKey (Just x)))) = [u "_id" =: x]
  toDocPF _ = []

instance (ToDoc f, ToDoc g) => ToDoc (f :+: g) where
  toDocPF (L x) = toDocPF x
  toDocPF (R x) = toDocPF x

instance (ToDoc f, ToDoc g) => ToDoc (f :*: g) where
  toDocPF (x :*: y) = toDocPF x ++ toDocPF y

instance (ToDoc f, Constructor c) => ToDoc (C c f) where
  toDocPF c@(C x) = toDocPF x ++ ["_cons" =: (u . conName) c]


------------------------------------------------------------------------------
-- | Convert arbitrary data type into 'Document'
toDoc :: (Regular a, ToDoc (PF a)) => a -> Document
toDoc x = toDocPF . from $ x


------------------------------------------------------------------------------
-- | A class that implements getting all the record labels in a list
--
class GetSelectors f where
  selsPF :: f r -> [String]

instance (Selector s, GetSelectors f) => GetSelectors (S s f) where
  selsPF s@(S x) = [selName s] ++ selsPF x

instance (GetSelectors f, GetSelectors g) => GetSelectors (f :+: g) where
  selsPF (L x) = selsPF x
  selsPF (R x) = selsPF x

instance (GetSelectors f, GetSelectors g) => GetSelectors (f :*: g) where
  selsPF (x :*: y) = selsPF x ++ selsPF y

instance (GetSelectors f) => GetSelectors (C c f) where
  selsPF (C x) = selsPF x


------------------------------------------------------------------------------
-- | A class that implements conversion of 'Document' objects into arbitrary
-- algebraic types.
--
class FromDoc f where
  fromDocPF :: Document -> Maybe (f a)


instance (Regular a, FromDoc (PF a)) => FromDoc (K a) where
  fromDocPF d = fromDoc d >>= return . K


instance (Val r, Selector s) => FromDoc (S s (K r)) where
  fromDocPF d = D.lookup k d >>= return . S . K
    where
      k = u . selName $ (undefined :: S s f a)


instance (Selector s) => FromDoc (S s (K RecKey)) where
  fromDocPF d = 
    case D.lookup "_id" d of
      Just x -> return . S . K . RecKey $ Just x
      Nothing -> return . S . K . RecKey $ Nothing


instance (Val r, Selector s) => FromDoc (S s (K (Optional r))) where
  fromDocPF d = 
    case D.lookup k d of
      Just x -> return . S . K . Optional $ Just x
      Nothing -> return . S . K . Optional $ Nothing
    where
      k = u . selName $ (undefined :: S s f a)

instance (Constructor c, FromDoc f) => FromDoc (C c f) where
  fromDocPF d = do
    cnm <- D.lookup "_cons" d
    case (cnm == conName (undefined :: C c f r)) of
      True -> fromDocPF d >>= return . C 
      False -> Nothing

instance (FromDoc f, FromDoc g) => FromDoc (f :+: g) where
  fromDocPF d = l `mplus` r
    where
      l = fromDocPF d >>= return . L
      r = fromDocPF d >>= return . R

instance (FromDoc f, FromDoc g) => FromDoc (f :*: g) where
  fromDocPF d = do
    x <- fromDocPF d
    y <- fromDocPF d
    return $ x :*: y

------------------------------------------------------------------------------
-- | Convert a 'Document' into arbitrary data type.
fromDoc :: (Regular a, FromDoc (PF a)) => Document -> Maybe a
fromDoc d = fromDocPF d >>= return . to




------------------------------------------------------------------------------
-- Testing


{-data Acme = ABC ByteString | One Int | Two Double | SomeDoc Document-}

{-$(deriveAll ''Acme "PFAcme")-}
{-type instance PF Acme = PFAcme-}

{-data Product = Product-}
  {-{ proId :: RecKey-}
  {-, proName :: ByteString-}
  {-, proCode :: Maybe ByteString-}
  {-, proAddFields :: Map ByteString ByteString-}
  {-, proOptField :: Optional ByteString-}
  {-} deriving (Eq, Show)-}

{-$(deriveAll ''Product "PFProduct")-}
{-type instance PF Product = PFProduct-}

{-someP = Product (RecKey Nothing)-}
                {-("Balta")-}
                {-(Just "101")-}
                {-(Map.singleton "Woohoo" "Yeehaa")-}
                {-(Optional Nothing)-}

{-someDocV1 = toDoc someP-}

{-someDocV2 = do-}
  {-oid <- genObjectId-}
  {-let p = someP { proId = RecKey (Just oid) }-}
  {-return $ toDoc p-}


{-sampleDoc = -}
  {-[ u "proId" =: (Nothing :: Maybe ByteString)-}
  {-, u "proName" =: ("Some product" :: ByteString)-}
  {-, u "proCode" =: Just ("Whatever123" :: ByteString)-}
  {-, u "proAddFields" =: (Map.fromList [] :: Map ByteString ByteString)-}
  {-, u "_cons" =: ("Product" :: ByteString)-}
  {-, u "proOptField" =: (123 :: Int)-}
  {-]-}

{-somePV1 :: Maybe Product-}
{-somePV1 = fromDoc sampleDoc-}

{-somePV2 :: IO (Maybe Product)-}
{-somePV2 = do-}
  {-oid <- genObjectId-}
  {-let s = ("_id" =: oid) : sampleDoc-}
  {-return $ fromDoc s-}
