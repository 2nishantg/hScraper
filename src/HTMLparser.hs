{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts #-}
module HTMLparser where
import Control.Monad (liftM, void)
import Control.Applicative ((<*))

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text


import Types

parseHtml :: T.Text -> Either ParseError HTMLTree
parseHtml s = case parse parseNodes "" (T.unwords (T.words s)) of
                Left err -> Left err
                Right nodes -> Right $
                  if length nodes == 1
                     then head nodes
                     else Types.toTree "html" [] nodes

parseNodes = spaces >> manyTill parseNode last
  where
    last = eof <|> void (try (string "</"))

parseNode = parseElement <|> parseText

parseText = liftM (Types.toLeaf . T.pack) $ many (noneOf "<")

parseElement = do
  -- opening tag
  (tag, attrs) <- between (char '<') (char '>') tagData
  -- contents
  children <- parseNodes
  -- closing tag
  string $ tag ++ ">" -- "</" is consumed by parseNodes, maybe bad form?
  return $ Types.toTree (T.pack tag) attrs children

tagData = do
  t <- tagName
  attrs <- attributes
  return (t,attrs)

tagName = many1 alphaNum

attributes =  spaces >> many (traillingSpaces attribute)

traillingSpaces a = a <* spaces

attribute = do
  name <- tagName
  char '='
  open <- char '\"' <|> char '\''
  value <- manyTill anyChar (try $ char open)
  return (T.pack name, T.pack value)

