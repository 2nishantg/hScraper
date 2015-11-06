import HScraper

main :: IO ()
main = do
  html <- parseSite "https://kat.cr/leopard-raws-taimadou-gakuen-35-shiken-shoutai-05-raw-sun-1280x720-x264-aac-mp4-t11528616.html/"
  let q1 = getParsedQuery "a[movieCover]"
  print $ html |>> q1
  let q2 = getParsedQuery "div[dataList]"
  let ans = head $ html |>> q2
  mapM_ (print .  getEntireText) (getList ans)


getList :: HTMLTree -> [HTMLTree]
getList (NTree _ xs) = xs
getList _ = []
