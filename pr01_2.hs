-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

myFST :: (a, b , c) -> a
myFST (a, _, _) = a

mySND :: (a, b, c) -> b
mySND (_, a, _) = a

myTHRD :: (a, b, c) -> c
myTHRD (_, _, a) = a

-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
myZipSave :: [a] -> [b] -> ([(a, b)], [a], [b])
myZipSave [] ys = ([], [], ys)
myZipSave xs [] = ([], xs, [])
myZipSave (x:xs) (y:ys) = ((x, y) : myFST res, mySND res, myTHRD res) where
    res = myZipSave xs ys

-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
myUnzipSave :: ([(a, b)], [a], [b]) -> ([a], [b])
myUnzipSave ([], restX, restY) = (restX, restY)
myUnzipSave ((x, y) : pairs, restX, restY) = (x : (fst res), y : (snd res)) where
    res = myUnzipSave (pairs, restX, restY)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f ini [] = ini
myFoldr f ini (x:xs) = x `f` myFoldr f ini xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f ini [] = ini
myFoldl f ini (x:xs) = myFoldl f (f ini x) xs

-- myReverse - разворот списка с использованием сверток
myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x:xs) = myFoldl f x xs
myFoldl1 _ [] = error "empty list"

-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [x] = x
myFoldr1 f (x:xs) = f x (myFoldr1 f xs)
myFoldr1 _ [] = error "empty list"

-- myTakeWhile - реализовать с использованием сверток
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (x:xs) = myFoldr f [] (x:xs) where
    f x lst = if pred x == True then x : lst else lst

-- mySpan - реализовать с использованием сверток
mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan _ [] = ([], [])
mySpan pred (x:xs) = myFoldr f ([], []) (x:xs) where
    f x (sat, unsat) | pred x && null unsat = (x : sat, unsat)
                     | otherwise            = (sat, x : unsat)

-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe deflt _ Nothing  = deflt
myMaybe _ f (Just x) = f x



{-
-- myUnFoldr - развертка (операция обратная к свертке)
MyUnfoldr :: (b -> (a, b)) -> b -> [a]
MyUnfoldr f ini = let (x, ini') = f ini in
    x: MyUnfoldr f ini'
-}


{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}
