import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import HScraper.Types
import HScraper.Show
import Data.Text as T
import Text.Parsec

main :: IO (Either ParseError HTMLTree)
main = parseSiteWithoutPrinting "http://home.iitk.ac.in/~nishgu/"


parseSite :: String ->  IO ()
parseSite url  = do
  str <- tidy =<< fetchResponse url
  print $ T.unwords (T.words str)
  print $  parseHtml str


parseSiteWithoutPrinting :: String ->  IO (Either ParseError HTMLTree)
parseSiteWithoutPrinting url  = do
  str <- tidy =<< fetchResponse url
  pure $ parseHtml str

