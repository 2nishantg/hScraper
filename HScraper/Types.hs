--All types should be declared here.
--TODO : XPath
module HScraper.Types where
import qualified Data.Text as T

data NodeType = Text T.Text
              | Element ElementData
              deriving (Show)

data NTree a = NTree a [NTree a]
             | NullTree
             deriving (Show)

type AttrList = [(T.Text , T.Text)]

data ElementData = ElementData T.Text AttrList
                 deriving (Show)

type HTMLTree = NTree NodeType

instance Eq ElementData where
  (ElementData x y) == (ElementData p q) = (x==p) && (y==q)

instance Eq NodeType where
  (Text x)    == (Text y)    = x==y
  (Element x) == (Element y) = x==y
  _ == _                     = False

instance (Eq a) => Eq (NTree a) where
  (NTree x y) == (NTree p q) = (x==p)&&(y==q)
  _ == _                     = False

toLeaf::T.Text -> HTMLTree
toLeaf t = NTree (Text t) []

toTree::T.Text -> AttrList -> [HTMLTree] -> HTMLTree
toTree t l = NTree (Element (ElementData t l))

type Name = T.Text

type Class = Maybe T.Text

type ID = Maybe T.Text

data NodeQuery = NodeQuery Name Class ID deriving (Show, Read)

type Query = [NodeQuery]
