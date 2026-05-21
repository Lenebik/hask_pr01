module Lib
    ( addMod
    , reverseWords
    , splitTokens
    , clean
    ) where

-- Приведение числа к остатку по модулю m через повторное вычитание/сложение.
normMod :: Int -> Int -> Int
normMod x m
    | x < 0     = normMod (x + m) m
    | x < m     = x
    | otherwise = normMod (x - m) m

-- Сложение двух чисел по модулю m
addMod :: Int -> Int -> Int -> Int
addMod x y m = normMod (x + y) m

-- Разбивает строку на чередующиеся блоки слов и пробелов
splitTokens :: String -> [String]
splitTokens [] = []
splitTokens (c:cs) =
    let isSpace = c == ' '
        (token, rest) = span (\x -> (x == ' ') == isSpace) (c:cs)
    in token : splitTokens rest

-- Убирает пустые токены
clean :: [String] -> [String]
clean = filter (not . null)

reverseWords :: String -> String
reverseWords = concat . reverse . splitTokens
