module HScraper.Network where

import Control.Monad


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import Data.Text.Encoding
import qualified Data.Text as T

import Network.HTTP.Conduit
import Network.HTTP.Types.Method

-- | A Simple wrapper around simpleHttp so that
-- it goes along with with other functions.
fetchResponse :: String -> IO T.Text
fetchResponse url  =  liftM  (decodeUtf8 . B.concat . BL.toChunks ) $  simpleHttp url
-- Don't know why it is necessary that URL ends with a /

parseParams :: [(String, String)] -> String
parseParams [] = ""
parseParams [(name, value)] = name ++ "=" ++ value
parseParams ((name, value) : xs) = name ++ "=" ++ value ++ "&" ++ parseParams xs

fetchGETRequest :: String -> [(String, String)] -> IO T.Text
fetchGETRequest url list  = do
  initReq <- parseUrl url
  let req = initReq
            { method = methodGet
            , queryString = Data.ByteString.Char8.pack $ parseParams list
            }
  manager <- newManager tlsManagerSettings
  res <- httpLbs req manager
  return $  decodeUtf8 . B.concat . BL.toChunks $ responseBody res
