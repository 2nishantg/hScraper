import System.IO
import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import HScraper.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = putStrLn "Hi"

getSourceFromFile :: String -> IO String
getSourceFromFile str = unwords . words  <$> ( openFile str ReadMode >>= hGetContents)

getFromFile :: String -> IO HTMLTree
getFromFile str = getParsedHTML <$>  parseHtml <$> (openFile str ReadMode >>= TIO.hGetContents)

applyOnIO :: (a -> b) -> IO a -> IO b
applyOnIO f a = f <$> a

tempHTMLTree :: IO HTMLTree
tempHTMLTree = getParsedHTML <$>  parseHtml <$> (tidy =<< fetchResponse "http://home.iitk.ac.in/~nishgu/")

getParsedHTML :: Either a HTMLTree -> HTMLTree
getParsedHTML = either (const NullTree) id

getParsedQuery :: Either a Query -> Query
getParsedQuery = either (const []) id

