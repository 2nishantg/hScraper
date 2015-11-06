module HScraper.Main where

import System.IO
import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import HScraper.Types
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


getSourceFromFile :: String -> IO String
getSourceFromFile str = unwords . words  <$> ( openFile str ReadMode >>= hGetContents)

getFromFile :: String -> IO HTMLTree
getFromFile str = getParsedHTML_ <$>  parseHtml <$> (openFile str ReadMode >>= TIO.hGetContents)

tempHTMLTree :: IO HTMLTree
tempHTMLTree = getParsedHTML_ <$>  parseHtml <$> (tidy =<< fetchResponse "http://home.iitk.ac.in/~nishgu/")

getParsedHTML_ :: Either a HTMLTree -> HTMLTree
getParsedHTML_ = either (const NullTree) id

getParsedQuery_ :: Either a Query -> Query
getParsedQuery_ = either (const []) id

-- | Takes a 'String' and tries to parse as 'Query'
-- returns empty query if fails.
getParsedQuery :: String -> Query
getParsedQuery str = getParsedQuery_  $ parseQuery str

