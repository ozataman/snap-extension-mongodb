{-|

'Snap.Extension.Timer' exports the 'MonadTimer' interface which allows you to
keep track of the time at which your application was started. The interface's
only operation is 'startTime'.

Two splices, 'startTimeSplice' and 'currentTimeSplice' are also provided, for
your convenience.

'Snap.Extension.Timer.Timer' contains the only implementation of this
interface and can be used to turn your application's monad into a
'MonadTimer'.

More than anything else, this is intended to serve as an example Snap
Extension to any developer wishing to write their own Snap Extension.

-}

module Snap.Extension.MongoDB
  ( MonadMongoDB(..)
  ) where

import           Control.Monad.Trans
import           Control.Monad.Reader

import           Database.MongoDB
import           Snap.Types


------------------------------------------------------------------------------
-- | The 'MonadMongoDB' class. Minimal complete definition:
class MonadSnap m => MonadMongoDB m where

  ----------------------------------------------------------------------------
  -- | 
  withDB :: ReaderT Database (Action m) a -> m (Either Failure a)
