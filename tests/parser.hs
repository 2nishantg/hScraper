import HScraper.HTMLparser
import HScraper.Network


main :: IO ()
main = parseSite "http://home.iitk.ac.in/~nishgu/"

parseSite :: String ->  IO ()
parseSite url  = do
  str <- fetchResponse url
  print str
  print $  parseHtml str
