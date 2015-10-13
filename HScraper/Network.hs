module HScraper.Network where

import Control.Monad


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Text.Encoding
import qualified Data.Text as T

import Network.HTTP.Conduit

-- | A Simple wrapper around simpleHttp so that
-- it goes along with with other functions.
fetchResponse :: String -> IO T.Text
fetchResponse url  =  liftM  (decodeUtf8 . B.concat . BL.toChunks ) $  simpleHttp url
-- Don't know why its necessary that URL ends with a /
