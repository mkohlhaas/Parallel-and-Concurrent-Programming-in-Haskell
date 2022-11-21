{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ConcurrentUtils
  ( -- * Variants of forkIO
    forkFinally,
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import GHC.Conc
import GHC.Exts
import GHC.IO hiding (finally)
import Prelude hiding (catch)

#if __GLASGOW_HASKELL__ < 706
-- | fork a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value.  The function is
-- called with asynchronous exceptions masked.
forkFinally ∷ IO a → (Either SomeException a → IO ()) → IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
