module HScraper.Query (
  parseQuery,
  (~=~),
  (<=>),
  (>=>)
  ) where

import qualified Data.Text as T

import HScraper.Types
import HScraper.QueryParser

(===) :: Eq a => Maybe a -> Maybe a -> Bool
Just x === Just y = x == y
_ === Nothing = True
Nothing === Just _ = False

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

-- | Applies '(>=>)' considering each node as root and
-- combines the result.
(<=>) :: HTMLTree -> Query -> [HTMLTree]
NullTree <=> _ = []
nt@(NTree _ xs) <=> q = foldl (\x y -> (y <=> q) `mappend` x) (nt >=> q) xs
