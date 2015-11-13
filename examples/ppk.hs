import HScraper
import Data.String.Utils
import Data.Text as T


main :: IO ()
main = do
  html <- parseSite "http://www.cse.iitk.ac.in/users/ppk/"
  let q1 = getParsedQuery "a"
  let list = html |>> q1
  let l' = fmap ( getAttribute "href" ) list
  printList $ Prelude.filter matchPost l'

printTree :: IO ()
printTree = showTree $  parseSite  "http://www.cse.iitk.ac.in/users/ppk/"


matchPost :: Maybe String -> Bool
matchPost (Just url) = startswith "./posts" url && (endswith ".html" url)
matchPost _ = False

printList :: [Maybe String] -> IO ()
printList [] = return ()
printList ((Just x):xs) = print x >> printList xs
printList (Nothing:xs) = printList xs
