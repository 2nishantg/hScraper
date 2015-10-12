import HScraper.HTMLparser
import qualified Data.Text as T
import Network.HTTP

testHtml :: IO String
testHtml = simpleHTTP (getRequest "http://home.iitk.ac.in/~nishgu") >>= getResponseBody

main :: IO ()
main = do
  str <- testHtml
  print str
  print $  parseHtml $ T.pack str
