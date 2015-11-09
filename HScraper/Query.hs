{- |
  A simple 'Query' format to query the 'HTMLTree'.

  The Syntax is as follows :
@
    "nodeName[Class(optional)]{ID(optional)} > nodeName[Class(optional)]{ID(optional)}"
@

eg : @"div{id1} > span[class][id_h1] > a"@

-}

module HScraper.Query (
  parseQuery,
  (~=~),
  (|>>),
  (>=>),
  getText,
  getEntireText,
  getAttribute
  ) where

import qualified Data.Text as T
import Data.Monoid
import HScraper.Types
import HScraper.QueryParser

(===) :: Eq a => Maybe a -> Maybe a -> Bool
Just x === Just y = x == y
_ === Nothing = True
Nothing === Just _ = False

-- | Compares 'NodeQuery' with a 'NodeType'.
(~=~) :: NodeQuery -> NodeType -> Bool
NodeQuery{} ~=~ Text _ = False
NodeQuery name cls idd ~=~ Element nm xs = (name == nm)
                                           && (lookup (T.pack "class") xs === cls)
                                           && (lookup (T.pack "id") xs === idd)

-- | Returns the list of nodes matching the query
-- with root matching the first NodeQuery, and subsequent
-- Children satisfying subsequent 'NodeQueries' continously.
(>=>) :: HTMLTree -> Query -> [HTMLTree]
NullTree >=>  _ = []
nt >=> [] = [nt]
nt@(NTree a _) >=> [q]
  | q ~=~ a = [nt]
  | otherwise = []
NTree a xs >=> (q:qs)
  | q ~=~ a = foldl g [] xs
  | otherwise = []
  where g acc l = acc `mappend` (l >=> qs)

-- | Applies '>=>' considering each node as root and
-- combines the result.
(|>>) :: HTMLTree -> Query -> [HTMLTree]
NullTree |>> _ = []
nt@(NTree _ xs) |>> q = foldl (\x y -> (y |>> q) `mappend` x) (nt >=> q) xs

-- | Get  Combined Text of immediate children of current node.
getText :: HTMLTree -> T.Text
getText NullTree = T.empty
getText nt@(NTree _ xs ) = foldl f (g nt) xs
  where
    f acc x = acc `T.append` g x
    g (NTree (Text x) _) = x
    g _ = T.empty

-- | Get Entire text contained in the subtree.
getEntireText :: HTMLTree -> T.Text
getEntireText NullTree = T.empty
getEntireText (NTree (Text x) _) = x
getEntireText (NTree (Element _ _) xs) = foldl fn T.empty  xs
  where
    fn acc x =  acc `T.append` gn x
    gn NullTree = T.empty
    gn (NTree (Text x) _) = x
    gn ntm@(NTree (Element _ _) _) = getEntireText ntm

-- | Get the value of an attribute of a node.
getAttribute :: String -> HTMLTree -> Maybe String
getAttribute  str (NTree (Element _ xs ) _) = fmap T.unpack (lookup (T.pack str) xs)
getAttribute _ _ = Nothing
