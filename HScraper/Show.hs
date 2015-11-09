module HScraper.Show (
  showTree,
  printTree
) where

import HScraper.Types
import HScraper.HTMLparser
import qualified Data.Text as T

printTree:: IO String -> IO () 
printTree x = do
  x' <- x
  putStrLn x'

showTree:: HTMLTree -> String
showTree x= (showTree' x $showString "") ""

showTree':: HTMLTree -> ShowS -> ShowS
showTree' (NTree (Text t) []) p = p . showChar '|' . showString (T.unpack t) . showString "\n"
showTree' (NTree (Element t a) l) p = p . showChar '|' . showString (T.unpack t) . showString "\n" . foldr (\x y -> showTree' x (p . showString "\t") . showString " " . y) (showString " ") l
