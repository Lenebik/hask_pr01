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

-- myUnFoldr - развертка (операция обратная к свертке)
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f ini = helper (f ini) where
    helper (Just (x,ini')) = x : myUnfoldr f ini'
    helper Nothing           = []

-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
data MyList a = MyEmpty | MyCons a (MyList a) deriving Show

-- myMap :: (a -> b) -> MyList a -> MyList b
-- myMap _ MyEmpty         = MyEmpty
-- myMap f (MyCons x xs)   = MyCons (f x) (myMap f xs)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-- Типы описания составляющих:
data Ingredient = Oil Int | Chocolate Int | Vanilla Int | Carrot Int | Egg Int | Flour Int | Shugar Int | BakingPowder Int deriving Show

data FillingMix = NotMix | SpoiledMix | OilChocolateMix | OilVanillaMix | OilCarrotMix deriving Show

data Dough = NotDough | SpoiledDough | CakeDough deriving Show

data CakeDoughType = NotCakeDough | SpoiledCakeDough | ChocolateCakeDough | VanillaCakeDough | CarrotCakeDough deriving Show

data Cake = NotCake | SpoiledCake | ChocolateCake | VanillaCake | CarrotCake deriving Show

data Action = Bake deriving Show

-- Приготовление смеси для начинки
makeCakeMix :: Ingredient -> Ingredient -> FillingMix
makeCakeMix (Oil x) (Chocolate y) | x >= 200 && y >= 20 = OilChocolateMix
makeCakeMix (Chocolate x) (Oil y) | x >= 20 && y >= 200 = OilChocolateMix
makeCakeMix (Oil x) (Vanilla y)   | x >= 200 && y >= 15 = OilVanillaMix
makeCakeMix (Vanilla x) (Oil y)   | x >= 15 && y >= 200 = OilVanillaMix
makeCakeMix (Oil x) (Carrot y)    | x >= 200 && y >= 50 = OilCarrotMix
makeCakeMix (Carrot x) (Oil y)    | x >= 50 && y >= 200 = OilCarrotMix
makeCakeMix (Oil x) (Oil y)       | x >= 200 && y >= 200 = SpoiledMix
makeCakeMix _ _                   = NotMix

-- Приготовление теста
cakeDough :: Ingredient -> Ingredient -> Ingredient -> Ingredient -> Dough
cakeDough (Egg e) (Flour f) (Shugar s) (BakingPowder b) 
    | e >= 8 && f >= 200 && s >= 100 && b >= 2 = CakeDough
cakeDough (Oil _) _ _ _ = SpoiledDough
cakeDough _ _ _ _       = NotDough

-- Создание типа теста для торта
chocolateCakeDough :: Dough -> FillingMix -> CakeDoughType
chocolateCakeDough SpoiledDough _ = SpoiledCakeDough
chocolateCakeDough _ SpoiledMix   = SpoiledCakeDough
chocolateCakeDough CakeDough OilChocolateMix = ChocolateCakeDough
chocolateCakeDough _ _ = NotCakeDough

vanillaCakeDough :: Dough -> FillingMix -> CakeDoughType
vanillaCakeDough SpoiledDough _ = SpoiledCakeDough
vanillaCakeDough _ SpoiledMix   = SpoiledCakeDough
vanillaCakeDough CakeDough OilVanillaMix = VanillaCakeDough
vanillaCakeDough _ _ = NotCakeDough

carrotCakeDough :: Dough -> FillingMix -> CakeDoughType
carrotCakeDough SpoiledDough _ = SpoiledCakeDough
carrotCakeDough _ SpoiledMix   = SpoiledCakeDough
carrotCakeDough CakeDough OilCarrotMix = CarrotCakeDough
carrotCakeDough _ _ = NotCakeDough

-- Выпекание торта
chocolateCake :: CakeDoughType -> Action -> Cake
chocolateCake SpoiledCakeDough _ = SpoiledCake
chocolateCake ChocolateCakeDough Bake = ChocolateCake
chocolateCake _ _ = NotCake

vanillaCake :: CakeDoughType -> Action -> Cake
vanillaCake SpoiledCakeDough _ = SpoiledCake
vanillaCake VanillaCakeDough Bake = VanillaCake
vanillaCake _ _ = NotCake

carrotCake :: CakeDoughType -> Action -> Cake
carrotCake SpoiledCakeDough _ = SpoiledCake
carrotCake CarrotCakeDough Bake = CarrotCake
carrotCake _ _ = NotCake

-- Примеры успешного приготовления
mixChoco = makeCakeMix (Oil 200) (Chocolate 20)
doughChoco = cakeDough (Egg 8) (Flour 200) (Shugar 100) (BakingPowder 2)
cakeDoughChoco = chocolateCakeDough doughChoco mixChoco
cakeChoco = chocolateCake cakeDoughChoco Bake

mixVanilla = makeCakeMix (Oil 200) (Vanilla 15)
doughVanilla = cakeDough (Egg 8) (Flour 200) (Shugar 100) (BakingPowder 2)
cakeDoughVanilla = vanillaCakeDough doughVanilla mixVanilla
cakeVanilla = vanillaCake cakeDoughVanilla Bake

mixCarrot = makeCakeMix (Oil 200) (Carrot 50)
doughCarrot = cakeDough (Egg 8) (Flour 200) (Shugar 100) (BakingPowder 2)
cakeDoughCarrot = carrotCakeDough doughCarrot mixCarrot
cakeCarrot = carrotCake cakeDoughCarrot Bake

-- Ошибки:
mixSpoil1 = makeCakeMix (Oil 50) (Chocolate 20)  -- недостаточно масла
mixNot1 = makeCakeMix (Egg 8) (Chocolate 20)  -- не те ингредиенты
doughSpoil = cakeDough (Oil 4) (Flour 200) (Shugar 100) (BakingPowder 2)  -- масло вместо яиц
cakeNotMatch = vanillaCake cakeDoughChoco Bake  -- несоответствие смеси и теста

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
