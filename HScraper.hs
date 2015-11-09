{- |
A library to parse, crawl and scrape webpages.

An example :

@
import HScraper

main :: IO ()
main = do
  html <- parseSite "https://kat.cr/leopard-raws-taimadou-gakuen-35-shiken-shoutai-05-raw-sun-1280x720-x264-aac-mp4-t11528616.html/"
  let q1 = getParsedQuery "a[movieCover]"
  print $ html |>> q1
  let q2 = getParsedQuery "a"
  let ans = html |>> q2
  mapM_ (print . getAttribute "href" ) ans -- get all hyperlinks.
@
-}

module HScraper (
  module HScraper.HTMLparser ,
  module HScraper.Network ,
  module HScraper.Query ,
  module HScraper.Types ,
  module HScraper.Tidy ,
  module HScraper.Main
  ) where


import HScraper.HTMLparser
import HScraper.Network
import HScraper.Query
import HScraper.Tidy
import HScraper.Types
import HScraper.Main

