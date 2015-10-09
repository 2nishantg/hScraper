--All types should be declared here.
--TODO : XPath
import qualified Data.Text as T

data NodeType = Text T.Text
              | Element ElementData
              deriving (Show)

data NTree a = NTree a [NTree a]
             deriving (Show)

type AttrList = [(String , String)]

data ElementData = ElementData T.Text AttrList
                 deriving (Show)

type HTMLTree = NTree NodeType
