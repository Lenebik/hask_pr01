module MyParsers.ParsL where

import MyTypes.MyMaybe
import Control.Applicative (Applicative(..), Alternative(..)) 
import Data.Char


newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a) }

instance Functor (Parser tok) where
   fmap g (Parser u) = Parser f where 
    -- f :: [tok] -> Maybe ([tok], b)
    f xs = case u xs of 
        MyNothing -> MyNothing
        MyJust (toks', x) -> MyJust (toks', g x)

instance Applicative (Parser tok) where
    pure x = Parser $ \xs -> MyJust(xs,x)
    Parser u <*> Parser v = Parser f where 
        f xs = case u xs of 
            MyNothing -> MyNothing
            MyJust (tok, x) -> case v tok of 
                MyNothing -> MyNothing
                MyJust (tok', x') -> MyJust (tok', x x')

instance Alternative (Parser tok) where
    empty = Parser $ \_ -> MyNothing 
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            MyNothing -> v xs
            x -> x

optional :: Alternative f => f a -> f (MyMaybe a)
optional v = MyJust <$> v <|> pure MyNothing

{-charA :: Parser Char Char
charA = Parser f where 
    f (x:xs) | x == 'a' = Just (xs, x)
    f _ = Nothing -}

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
--Применить умножить к числу digit (digit * ) /игнор * / digit -> (digit * /*/ digit)

lowers :: Parser Char String
lowers = (:) <$> lower <*> lowers <|> pure ""

digits :: Parser Char Int
digits = foldl (\x y -> x * 10 + y) 0 <$> some digit
--применяем ко всем цифрам в строке функцию

finalMult :: Parser Char Int
finalMult = (*) <$> digits <* char '*' <*> digits

finalPlus :: Parser Char Int
finalPlus = (+) <$> digits <* char '+' <*> digits

plusOrMult :: Parser Char Int
plusOrMult = finalMult <|> finalPlus