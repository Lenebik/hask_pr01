-------------------------------------
-- Практическое задание 2. Часть 3 --
-------------------------------------

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

module MyParsers.MyParser (
    Parser, runParser, satisfy, char, lower, digit, multiplication, 
    lowers, digits, finalMult, finalPlus, plusOrMult
) where

import MyTypes.MyMaybe
import Control.Applicative (Alternative(..)) 
import Data.Char

newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a) }

instance Functor (Parser tok) where

    fmap g (Parser u) = Parser f where
        -- f :: [tok] -> Maybe ([tok], b)
        f xs = case u xs of
            MyNothing -> MyNothing
            MyJust (toks', x) -> MyJust (toks', g x)

instance Applicative (Parser tok) where

    -- pure :: a -> Parser tok a
    pure x = Parser $ \xs -> MyJust(xs,x)

    -- (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser u <*> Parser v = Parser f where 
        f xs = case u xs of 
            MyNothing -> MyNothing
            MyJust (tok, x) -> case v tok of 
                MyNothing -> MyNothing
                MyJust (tok', x') -> MyJust (tok', x x')

instance Alternative (Parser tok) where
    --empty :: Parser tok a
    empty = Parser $ \_ -> MyNothing 

    --(<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            MyNothing -> v xs
            x -> x

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy с = Parser f where 
    f (x:xs) | с x = MyJust (xs, x)
    f _ = MyNothing 

char :: Char -> Parser Char Char
char c  = satisfy (== c)
-- runParser (char 'a') "asd"

lower :: Parser Char Char
lower = satisfy isLower

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Char Int 
multiplication = (*) <$> digit <* char '*' <*> digit 

lowers :: Parser Char String
lowers = (:) <$> lower <*> lowers <|> pure ""

digits :: Parser Char Int
digits = foldl (\x y -> x * 10 + y) 0 <$> some digit

finalMult :: Parser Char Int
finalMult = (*) <$> digits <* char '*' <*> digits

finalPlus :: Parser Char Int
finalPlus = (+) <$> digits <* char '+' <*> digits

plusOrMult :: Parser Char Int
plusOrMult = finalMult <|> finalPlus