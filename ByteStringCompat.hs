{-# LANGUAGE CPP #-}

module ByteStringCompat () where

import Control.DeepSeq ()
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

#if !MIN_VERSION_bytestring(0,10,0)

instance NFData ByteString where
  rnf x = B.length x `seq` ()

#endif
