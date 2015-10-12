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

toLeaf::T.Text -> HTMLTree
toLeaf t = NTree (Text t) []

toTree::T.Text -> AttrList -> [HTMLTree] -> HTMLTree
toTree t l tree = NTree (Element (ElementData t l)) tree
