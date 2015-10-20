-- | The Syntax is as follows :
-- "nodeName[Class(optional)]{ID(optional)} > nodeName[Class(optional)]{ID(optional)}"
-- eg : "div{id1} > h1[class]"
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts #-}
module HScraper.QueryParser (
  parseQuery
  ) where

import qualified Data.Text as T
import Text.Parsec

import HScraper.Types

parseQuery :: String -> Either ParseError Query
parseQuery s = case parse parseNodeQueries "" (unwords (words s)) of
  Left err -> Left err
  Right nodes -> Right nodes


parseNodeQueries :: Stream s m Char => ParsecT s u m [NodeQuery]
parseNodeQueries = sepBy node (spaces >> char '>' >> spaces)

clas :: Stream s m Char => ParsecT s u m String
clas = do
  _ <- char '['
  clnm <- many (noneOf "]")
  _ <- char ']'
  return clnm

idd :: Stream s m Char => ParsecT s u m String
idd = do
  _ <- char '{'
  idnm <- many (noneOf "}")
  _ <- char '}'
  return idnm

node :: Stream s m Char => ParsecT s u m NodeQuery
node = do
  name <-many (noneOf "{[>")
  cls <- optionMaybe clas
  ids <- optionMaybe idd
  return (NodeQuery (T.pack name) (fmap T.pack cls) (fmap T.pack ids))
