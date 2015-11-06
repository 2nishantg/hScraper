module HScraper.Main (
  getFromFile,
  getParsedHTML,
  getParsedQuery,
  parseSite
  ) where

import System.IO
import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import HScraper.Types

import Control.Applicative
import Data.Monoid

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

-- | Tries to parse html from file. returns NullTree
-- if parsing fails.
getFromFile :: FilePath -> IO HTMLTree
getFromFile str = getParsedHTML_ <$>  parseHtml <$> (openFile str ReadMode >>= TIO.hGetContents)

tempHTMLTree :: IO HTMLTree
tempHTMLTree = getParsedHTML_ <$>  parseHtml <$> (tidy =<< fetchResponse "http://home.iitk.ac.in/~nishgu/")

getParsedHTML_ :: Either a HTMLTree -> HTMLTree
getParsedHTML_ = either (const NullTree) id

-- | like 'parseHtml' but returns 'NullTree'
-- if parsing fails.
getParsedHTML :: T.Text -> HTMLTree
getParsedHTML = getParsedHTML_ . parseHtml

getParsedQuery_ :: Either a Query -> Query
getParsedQuery_ = either (const []) id

-- | Takes a 'String' and tries to parse as 'Query'
-- returns empty query if parsing fails.
getParsedQuery :: String -> Query
getParsedQuery = getParsedQuery_ .  parseQuery

parseSite :: String -> IO HTMLTree
parseSite url  = do
  str <- tidy =<< fetchResponse url
  return $  getParsedHTML str


