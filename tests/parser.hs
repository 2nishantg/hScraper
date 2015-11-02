import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import Data.Text as T

main :: IO ()
main = parseSiteWithoutPrinting "http://home.iitk.ac.in/~nishgu/"


parseSite :: String ->  IO ()
parseSite url  = do
  str <- tidy =<< fetchResponse url
  print $ T.unwords (T.words str)
  print $  parseHtml str


parseSiteWithoutPrinting :: String ->  IO ()
parseSiteWithoutPrinting url  = do
  str <- tidy =<< fetchResponse url
  print $  parseHtml str
