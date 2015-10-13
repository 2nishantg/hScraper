{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts #-}
module HScraper.HTMLparser where
import Control.Monad (liftM, void)
import Control.Applicative ((<*))

import Data.List (nub)
import qualified Data.Text as T
import Text.Parsec



import HScraper.Types

parseHtml :: T.Text -> Either ParseError HTMLTree
parseHtml s = case parse parseNodes "" (T.unwords (T.words s)) of
                Left err -> Left err
                Right nodes -> Right $
                 if length nodes == 1
                     then head nodes
                     else case filter filterHelper nodes of
                            [] -> toTree "html" [] nodes
                            x  -> head x

filterHelper::HTMLTree -> Bool
filterHelper (NTree x y) = case x of
                            Element p -> case p of
                                           ElementData x y -> x=="html"
                                           -- _               -> False
                            _         -> False

comments = do
  spaces
  string "<!"
  manyTill anyChar (char '>')
  return ()

parseNodes = do
  void (try comments) <|> spaces
  manyTill parseNode last
  where
    last = eof <|> void (try (string "</"))

parseNode = parseElement <|> parseText

parseText = fmap (toLeaf . T.pack) $ many (noneOf "<")

parseElement = do
  (tag, attrs) <- between (char '<') (char '>') tagData
  children <- parseNodes
  string $ tag ++ ">"
  return $ toTree (T.pack tag) attrs $nub children

tagData = do
  t <- tagName
  attrs <- attributes
  return (t,attrs)

tagName = many1 alphaNum

attributes =  spaces >> many (traillingSpaces attribute)

traillingSpaces = (<* spaces)

attribute = do
  name <- tagName
  char '='
  open <- char '\"' <|> char '\''
  value <- manyTill anyChar (try $ char open)
  return (T.pack name, T.pack value)

