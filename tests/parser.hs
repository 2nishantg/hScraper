import HScraper.HTMLparser
import HScraper.Network


main :: IO ()
main = do
  str <- fetchResponse "http://home.iitk.ac.in/~nishgu/"
  print str
  print $  parseHtml str
