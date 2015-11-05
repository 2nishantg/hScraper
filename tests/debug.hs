import System.IO
import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import HScraper.Types
import qualified Data.Text as T

main = putStrLn "Hi"

getStringFromFile :: String -> IO String
getStringFromFile str = unwords . words  <$> ( openFile str ReadMode >>= hGetContents)

applyOnIO :: (a -> b) -> IO a -> IO b
applyOnIO f a = f <$> a

tempHTMLTree :: IO HTMLTree
tempHTMLTree = getParsedHTML <$>  parseHtml <$> (tidy =<< fetchResponse "http://home.iitk.ac.in/~nishgu/")

getParsedHTML :: Either a HTMLTree -> HTMLTree
getParsedHTML = either (const NullTree) id

getParsedQuery :: Either a Query -> Query
getParsedQuery = either (const []) id 
