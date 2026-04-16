{-

1. Создайте и настройте stack проект myParser

2. Создайте в src директорию MyTypes и создайте в нем три модуля со следующими реализациями:
- тип MyEither, аналогичный стандартному Either. Сделайте этот тип представителем классов типов Foldable, Semigroup, Functor и Applicative (*)
(*) для других необходимых классов типов можно использовать механизм deriving

Добавьте в конец каждого файла многострочный комментарий: 
для реализованного типа и определенных представителей классов типов напишите пример вызова соответствующих функций и результатов работы из ghci 
(если класс типов не требуется определять по заданию, то пример вызова не нужен):
- fold, foldMap, foldr
- (<>), sconcat, stimes
- mappend, mconcat
- fmap, (<$)
- pure, (<*>), liftA2, (*>), (<*)

3. Импортируйте модули в Main.hs

-}

module MyTypes.MyEither  ( MyEither(MyLeft, MyRight) ) where

data MyEither e a = MyLeft e | MyRight a deriving (Show, Eq, Read)

instance Functor (MyEither e) where
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight x) = MyRight (f x)

instance Foldable (MyEither e) where
    foldr _ z (MyLeft _) = z
    foldr f z (MyRight x) = f x z

    foldMap _ (MyLeft _) = mempty
    foldMap f (MyRight x) = f x

instance Applicative (MyEither e) where
    pure = MyRight

    MyLeft e <*> _ = MyLeft e
    _ <*> MyLeft e = MyLeft e
    MyRight f <*> MyRight x = MyRight (f x)

{-

Functor
    ghci> fmap (*2) (MyRight 5)
    MyRight 10

    ghci> fmap (+4) (MyLeft "error")
    MyLeft "error"

    ghci> 100 <$ (MyRight 5)
    MyRight 100

    ghci> 100 <$ (MyLeft "error")
    MyLeft "error"

Foldable

    ghci> foldr (+) 10 (MyRight 5)
    15

    ghci> foldr (+) 10 (MyLeft "error")
    10

    ghci> foldMap Sum (MyRight 5)
    Sum {getSum = 5}

    ghci> foldMap Sum (MyLeft "error")
    Sum {getSum = 0}

    ghci> fold (MyRight (Sum 3))
    Sum {getSum = 3}

    ghci> fold (MyLeft "error")
    ()

Applicative

    ghci> pure 42
    42

    ghci> MyRight (*7) <*> MyRight 6
    MyRight 42

    ghci> MyRight (*7) <*> MyLeft "error"
    MyLeft "Error"

    ghci> liftA2 (+) (MyRight 3) (MyRight 7)
    MyRight 10

    liftA2 (+) (MyLeft "error") (MyRight 7)
    MyLeft "error"

    ghci> pure 1  *> (MyRight 5)
    MyRight 5

    pure 1 <* (MyRight 5)
    MyRight 1
-}