module HScraper.Show (
  showTree
) where

import HScraper.Types
import HScraper.HTMLparser
import qualified Data.Text as T

-- | takes a 'HTMLtree' and prints it in a neat manner.
--
--  expected output
--
--  @
--  |html
--         |head
--         |body
--                YOLO
--  @
--
showTree:: IO HTMLTree -> IO ()
showTree y= do
  x  <- y
  let x' = (showTree' x $showString "") ""
  putStrLn x'

showTree':: HTMLTree -> ShowS -> ShowS
showTree' (NTree (Text t) []) p = p . showString (T.unpack t) . showString "\n"
showTree' (NTree (Element t a) l) p = p . showChar '|' . showString (T.unpack t) . showString "\n" . foldr (\x y -> showTree' x (p . showString "\t") . showString " " . y) (showString " ") l
