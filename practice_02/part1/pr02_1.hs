-------------------------------------
-- Практические задание 2. Часть 1 --
-------------------------------------

module Pr02_1 where

{-

1. Создайте и настройте проект MyEvolProject в Cabal
Добавьте дополнительную зависимость в проект: random
Создайте новый модуль MyEvolModule.hs в папке app



2. Задайте тип-сумму MyEvolution в MyEvolModule.hs со следующими конструкторами и соответствующими строками (для класса типов Show):

Конструктор данных  | Соответствующая строка для классов типов Show
--------------------+---------------------------------------------- 
LUCA                | "Last Universal Common Ancestor"
Cyanobacteria       | "Synechococcus"
Trilobite           | "Paradoxides"
Ichthyostega        | "Ichthyostega"
Dimetrodon          | "Dimetrodon"
Archaeopteryx       | "Archaeopteryx"
Morganucodon        | "Morganucodon"
Purgatorius         | "Purgatorius"
Australopithecine   | "Australopithecus Afarensis"
Humans              | "Homo Sapiens"

Напишите вручную представителей классов типов: Show, Read, Eq, Ord, Enum, Bounded
Напишите аналогичный тип-сумму MyEvolution' и используя механизм deriving сделайте его представителем классов типов: Show, Read, Eq, Ord, Enum, Bounded

Добавьте в функцию main файла Main.hs вывод отсортированного списка значений типа MyEvolution. Используйте следующий код: putStrLn $ show $ sort ([...] :: [MyEvolution])

-}
 
