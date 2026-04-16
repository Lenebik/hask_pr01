{-

1. Создайте и настройте stack проект myParser

2. Создайте в src директорию MyTypes и создайте в нем три модуля со следующими реализациями:
- тип MyTree, который содержит значения в узлах и листьях. Сделайте этот тип представителем классов типов Foldable, Functor и Applicative (*)
(*) для других необходимых классов типов можно использовать механизм deriving

Добавьте в конец каждого файла многострочный комментарий: 
для реализованного типа и определенных представителей классов типов напишите пример вызова соответствующих функций и результатов работы из ghci 
- fold, foldMap, foldr
- fmap, (<$)
- pure, (<*>), liftA2, (*>), (<*)

3. Импортируйте модули в Main.hs

-}


module MyTypes.MyTree ( MyTree(Leaf, Node) ) where

data MyTree a = Leaf a | Node a (MyTree a) (MyTree a) deriving (Show, Eq, Read)

-- применение чистой функции к значению в контейнере
instance Functor MyTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- сворачиваемость структуры
instance Foldable MyTree where
    foldr f z (Leaf x) = f x z
    foldr f z (Node x left right) = f x (foldr f (foldr f z right) left)
    
    foldMap f (Leaf x) = f x
    foldMap f (Node x left right) = f x <> foldMap f left <> foldMap f right

    length (Leaf _)  = 1
    length (Node _ l r) = length l + length r + 1

-- приминение функции в контейнере к значению в контейнере
instance Applicative MyTree where
    pure = Leaf

    (Leaf g) <*> x = fmap g x
    (Node f leftF rightF) <*> (Leaf x) = Node (f x) (leftF <*> Leaf x) (rightF <*> Leaf x)
    (Node f leftF rightF) <*> (Node x left right) = Node (f x) (leftF <*> left) (rightF <*> right)
    
{-
Functor:

    ghci> fmap (+4) (Leaf 3)
    Leaf 7

    ghci> fmap (*2) (Node 1 (Leaf 2) (Leaf 3))
    Node 2 (Leaf 4) (Leaf 6)

    ghci> 100 <$ (Leaf 5)
    Leaf 100

    ghci> 100 <$ (Node 1 (Leaf 2) (Leaf 3))
    Node 100 (Leaf 100) (Leaf 100)

Foldable:

    Foldable:ghci> foldr (*) 1 (Node 1 (Leaf 2) (Leaf 3))
    6

    ghci> getProduct $ foldMap Sum (Node 1 (Leaf 2) (Leaf 4))
    8

    ghci> length (Node 5 (Leaf 2) (Leaf 3))
    3

    ghci> fold (Leaf (Sum 10))
    Sum {getSum = 10}

    ghci> fold (Node (Sum 1) (Leaf (Sum 2)) (Leaf (Sum 3)))
    Sum {getSum = 6}

Applicative:

    ghci> pure 5
    5

    ghci> Leaf (+10) <*> Node 2 (Leaf 4) (Leaf 6)
    Node 12 (Leaf 14) (Leaf 16)

    ghci> (Node (+2) (Leaf (+4)) (Leaf (+6))) <*>  (Node 2 (Leaf 4) (Leaf 6)) 
    Node 4 (Leaf 8) (Leaf 12)

    ghci> liftA2 (+) (Node 1 (Leaf 2) (Leaf 3)) (Node 10 (Leaf 20) (Leaf 30))
    Node 11 (Leaf 22) (Leaf 33)

    ghci> liftA2 (*) (Leaf 5) (Leaf 3)
    Leaf 15

    ghci> pure 1 *> (Node 1 (Leaf 2) (Leaf 3))
    Node 1 (Leaf 2) (Leaf 3)

    ghci> pure 1  <* (Node 1 (Leaf 2) (Leaf 3))
    Node 1 (Leaf 1) (Leaf 1)

-}
