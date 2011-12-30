## Snap Extension for MongoDB

NOTE: This project is now deprecated.  The old snap extension system has been
replaced with a new system called snaplets.  Check out the
[snaplet directory](http://snapframework.com/snaplets) for possible alternatives.

This package provides a straightforward way to integrate MongoDB database
connectivity into Snap applications.

The API is a work in progress, so expect changes.

Starting with 0.3, this library provides out-of-box integration with Snap.Auth.



### Generics Support

Snap.Extension.DB.MongoDB.Generics module provides generics support to
facilitate conversion of your data structures to and from Document types.


#### Example Generics Usage


    import Snap.Extension.DB.MongoDB.Generics

    data Product = Product
      { proId :: RecKey
      , proName :: ByteString
      , proCode :: Maybe ByteString
      , proAddFields :: Map ByteString ByteString
      , proOptField :: Optional ByteString
      } deriving (Eq, Show)

    $(deriveAll ''Product "PFProduct")
    type instance PF Product = PFProduct

    someP = Product (RecKey Nothing)
                    ("Balta")
                    (Just "101")
                    (Map.singleton "Woohoo" "Yeehaa")
                    (Optional Nothing)

    someDocV1 = toDoc someP

    someDocV2 = do
      oid <- genObjectId
      let p = someP { proId = RecKey (Just oid) }
      return $ toDoc p


    sampleDoc = 
      [ u "proId" =: (Nothing :: Maybe ByteString)
      , u "proName" =: ("Some product" :: ByteString)
      , u "proCode" =: Just ("Whatever123" :: ByteString)
      , u "proAddFields" =: (Map.fromList [] :: Map ByteString ByteString)
      , u "_cons" =: ("Product" :: ByteString)
      , u "proOptField" =: (123 :: Int)
      ]

    somePV1 :: Maybe Product
    somePV1 = fromDoc sampleDoc

    somePV2 :: IO (Maybe Product)
    somePV2 = do
      oid <- genObjectId
      let s = ("_id" =: oid) : sampleDoc
      return $ fromDoc s



