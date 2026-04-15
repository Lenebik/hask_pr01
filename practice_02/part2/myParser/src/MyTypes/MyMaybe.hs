{-

1. Создайте и настройте stack проект myParser

2. Создайте в src директорию MyTypes и создайте в нем три модуля со следующими реализациями:
- тип MyMaybe, аналогичный стандартному Maybe. Сделайте этот тип представителем классов типов Foldable, Semigroup, Monoid, Functor и Applicative (*)
(*) для других необходимых классов типов можно использовать механизм deriving

Добавьте в конец каждого файла многострочный комментарий: 
для реализованного типа и определенных представителей классов типов напишите пример вызова соответствующих функций и результатов работы из ghci 

3. Импортируйте модули в Main.hs

-}

module MyTypes.MyMaybe ( MyMaybe(MyJust, MyNothing) ) where

data MyMaybe a = MyJust a | MyNothing deriving (Show, Eq, Read)

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust a) = MyJust (f a)

instance Applicative MyMaybe where
    pure = MyJust

    MyJust f  <*> m = fmap f m
    MyNothing <*> m = MyNothing

instance Semigroup a => Semigroup (MyMaybe a) where
    MyNothing <> b = b
    a <> MyNothing = a
    MyJust a <> MyJust b = MyJust( a <> b)

instance Semigroup a => Monoid (MyMaybe a) where
    mempty = MyNothing

instance Foldable MyMaybe where 
    foldr _ z MyNothing = z
    foldr f z (MyJust x) = f x z

    foldMap _ MyNothing = mempty
    foldMap f (MyJust x) = f x

{-
Functor

    ghci> fmap (+1) (MyJust 5)
    MyJust 6

    ghci> fmap (*2) MyNothing
    MyNothing

Applicative

    ghci> pure 42
    42

    ghci> MyJust (*3) <*> MyJust 10
    MyJust 30

    ghci> MyNothing <*> MyJust 10
    MyNothing

Semigroup/Monoid

    ghci> MyJust (Sum 2) <> MyJust (Sum 3)
    MyJust (Sum {getSum = 5})

    ghci> MyNothing <> (MyJust (Sum 4))
    MyJust (Sum {getSum = 4})

    ghci> MyJust (Sum {getSum = 4})
    MyJust (Sum {getSum = 4})

    ghci> MyNothing <> MyNothing
    yNothing

Foldable

    ghci> foldr (*) 2 MyNothing
    2

    ghci> foldr (*) 2 (MyJust 5)    
    10

    ghci> foldMap Sum MyNothing
    Sum {getSum = 0}

    ghci> foldMap Sum (MyJust 10)
    Sum {getSum = 10}

-}