import HScraper.HTMLparser
import HScraper.Network
import Data.Text as T

main :: IO ()
main = parseSite "http://home.iitk.ac.in/~nishgu/"


parseSite :: String ->  IO ()
parseSite url  = do
  str <- fetchResponse url
  print $T.unwords (T.words str)
  print $  parseHtml str


parseSiteWithoutPrinting :: String ->  IO ()
parseSiteWithoutPrinting url  = do
  str <- fetchResponse url
  -- print str
  print $  parseHtml str
