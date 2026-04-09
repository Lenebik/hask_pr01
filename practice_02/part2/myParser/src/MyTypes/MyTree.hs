module MyTypes.MyTree ( MyTree(Leaf, Node) ) where

data MyTree a = Leaf a 
              | Node a (MyTree a) (MyTree a)
              deriving (Show, Eq, Read)

instance Functor MyTree where
    fmap f (Leaf x) = Leaf (f x)
    
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable MyTree where
    foldr f z (Leaf x) = f x z
    foldr f z (Node x left right) = 
        f x (foldr f (foldr f z right) left)
    
    foldMap f (Leaf x) = f x
    foldMap f (Node x left right) = 
        f x <> foldMap f left <> foldMap f right