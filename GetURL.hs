-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- Simple wrapper around HTTP, allowing proxy use

module GetURL (getURL) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit (simpleHttp)

getURL ∷ String → IO ByteString
getURL url = L.toStrict <$> simpleHttp url
