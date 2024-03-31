module BinaryTree where
import Test.QuickCheck (Arbitrary)

-- |A binary tree 
data BTree a = Leaf
               | Node (BTree a) a (BTree a)
      deriving (Show)

-- |Adds a new element to a given binary tree and returns the result
insert :: Ord a => BTree a -> a -> BTree a
insert Leaf             a                 = Node Leaf a Leaf
insert (Node  l v r)    a     | a == v    = Node l a r
                              | a < v     = Node (insert l a) v r
                              | a > v     = Node r v (insert r a)
insert _                _                 = Leaf

-- |returns an element from the binary tree equal to one given 
getElem :: (Ord a) => BTree a -> a -> Maybe a
getElem Leaf _ = Nothing
getElem (Node l v r) a | a==v= Just v
                       | a< v= getElem l a
                       | a> v= getElem r a
getElem _ _       = Nothing 

