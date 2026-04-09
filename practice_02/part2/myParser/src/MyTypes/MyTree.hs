module MyTypes.MyTree ( MyTree(Leaf, Node) ) where

data MyTree a = Leaf a 
              | Node a (MyTree a) (MyTree a)
              deriving (Show, Eq, Read)

instance Functor MyTree where
    fmap f (Leaf x) = Leaf (f x)
    
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)