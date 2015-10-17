{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts #-}
module HScraper.HTMLparser (
  parseHtml
  ) where
import Control.Monad (void)


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
filterHelper (NTree (Element (ElementData x _)) _ ) | x == "html" = True
filterHelper _ = False

comments :: Stream s m Char => ParsecT s u m ()
comments = do
  spaces
  _ <- string "<!--"
  _ <- manyTill anyChar (string "-->")
  return ()

parseNodes :: Stream s m Char => ParsecT s u m [HTMLTree]
parseNodes = do
  void (try comments) <|> spaces
  manyTill parseNode last'
  where
    last' = eof <|> void (try (string "</"))

parseNode :: Stream s m Char => ParsecT s u m HTMLTree
parseNode = parseElement <|> parseText

parseText :: Stream s m Char => ParsecT s u m HTMLTree
parseText =  (toLeaf . T.pack) <$> many (noneOf "<")

parseElement :: Stream s m Char => ParsecT s u m HTMLTree
parseElement = do
  (tag, attrs) <- between (char '<') (char '>') tagData
  children <- parseNodes
  _ <- string $ tag ++ ">"
  return $ toTree (T.pack tag) attrs $nub children

tagData :: Stream s m Char => ParsecT s u m (String, [(T.Text, T.Text)])
tagData = do
  t <- tagName
  attrs <- attributes
  return (t,attrs)

tagName :: Stream s m Char => ParsecT s u m String
tagName = many1 alphaNum

attributes :: Stream s m Char => ParsecT s u m [(T.Text, T.Text)]
attributes =  spaces >> many (trailingSpaces attribute)

trailingSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
trailingSpaces = (<* spaces)

attribute :: Stream s m Char => ParsecT s u m (T.Text, T.Text)
attribute = do
  name <- tagName
  _ <- char '='
  open <- char '\"' <|> char '\''
  value <- manyTill anyChar (try $ char open)
  return (T.pack name, T.pack value)

