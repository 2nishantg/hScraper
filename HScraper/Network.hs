{- |
Module for fetching different requests over network.
-}

module HScraper.Network
       ( fetchResponse
       , parseParams
       , fetchRequestWith
       , defaultGETRequest
       , fetchGETRequest
       , defaultPOSTRequest
       , fetchPOSTRequest
       ) where

import Control.Monad

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import Data.Text.Encoding
import qualified Data.Text as T

import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

-- | A Simple wrapper around simpleHttp so that
-- it goes along with with other functions.
fetchResponse :: String -> IO T.Text
fetchResponse url  =  liftM  (decodeUtf8 . B.concat . BL.toChunks ) $  simpleHttp url

-- | Parses a list of (key,value) pairs to pass with GET and POST requests.
parseParams :: [(String, String)] -> String
parseParams [] = ""
parseParams [(name, value)] = name ++ "=" ++ value
parseParams ((name, value) : xs) = name ++ "=" ++ value ++ "&" ++ parseParams xs

-- | takes a Request and a Manager and returns the response.
-- Useful when querying with a modified request.
fetchRequestWith :: Request -> Manager -> IO T.Text
fetchRequestWith req manager = decodeUtf8 . B.concat . BL.toChunks . responseBody <$> httpLbs req manager

-- | Minimal GET request, modify it to add proxy,cookies,user-agent etc.
-- Then fetch using @fetchRequestWith.
defaultGETRequest :: String -> [(String,String)] -> IO Request
defaultGETRequest url list = do
  initReq <- parseUrl url
  return initReq { method = methodGet
                 , queryString = pack $ parseParams list
                 }

-- | Fetches request using @defaultGETRequest.
fetchGETRequest :: String -> [(String, String)] -> IO T.Text
fetchGETRequest url list  = do
  req <- defaultGETRequest url list
  man <- newManager tlsManagerSettings
  res <- httpLbs req man
  return $  decodeUtf8 . B.concat . BL.toChunks $ responseBody res

-- | Minimal POST request, modify it to add proxy,cookies,user-agent etc.
-- Then fetch using @fetchRequestWith.
defaultPOSTRequest :: String -> [(String,String)] -> IO Request
defaultPOSTRequest url list = do
  initReq <- parseUrl url
  return initReq { method = methodPost
                 , requestBody = RequestBodyBS $  Data.ByteString.Char8.pack $ parseParams list
                 , requestHeaders = [ (hContentType, pack "application/x-www-form-urlencoded" ) ]
                 }

-- | Fetches request using @defaultPOSTRequest.
fetchPOSTRequest :: String -> [(String, String)] -> IO T.Text
fetchPOSTRequest url list  = do
  req <- defaultPOSTRequest url list
  manager <- newManager tlsManagerSettings
  res <- httpLbs req manager
  return $  decodeUtf8 . B.concat . BL.toChunks $ responseBody res

