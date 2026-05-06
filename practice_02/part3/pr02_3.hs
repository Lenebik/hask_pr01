-------------------------------------
-- Практическое задание 2. Часть 3 --
-------------------------------------

module Pr02_3 where

{-

Расширьте проект myParser созданный в практическом задании 2 части 2 
- создайте в src директорию MyParsers для файлов этого задания
- скопируйте реализацию парсера из лекции в новый модуль. Модифицируйте разработанный на лекции парсер заменив стандартный тип Maybe на собственный тип MyMaybe из 2-ой части 2-го задания
- создайте два новых модуля и в каждом из них реализуйте функционал повторяющий лекционный материал, но с использованием библиотек Parsec и Attoparsec
- изолируйте решения друг от друга и вызовите их через точку входа в проекте используя следующий код (с точностью до квалифицированного импорта):

main :: IO ()
main = do
    putStrLn "MyParser:"
    putStrLn $ show (runParser plusOrMult "12*345dsf")
    putStrLn $ show (runParser plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    putStrLn $ show (runParser plusOrMultParsec "12*345dsf")
    putStrLn $ show (runParser plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    putStrLn $ show (runParser plusOrMultAttoparsec "12*345dsf")
    putStrLn $ show (runParser plusOrMultAttoparsec "12+345dsf")

-}

