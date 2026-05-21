import Test.QuickCheck
import Lib

-- Положительный модуль
genMod :: Gen Int
genMod = choose (1, 1000)

-- Слово
genWord :: Gen String
genWord = listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

-- Блок из 1–3 пробелов
genSpaces :: Gen String
genSpaces = (`replicate` ' ') <$> choose (1, 3)

-- Фраза из нескольких слов
genPhrase :: Gen String
genPhrase = do
    n  <- choose (2, 6)
    ws <- vectorOf n genWord
    return (unwords ws)

genText :: Gen String
genText = concat <$> listOf1 (oneof [genWord, genSpaces])

-- (addMod x y m) `mod` m == (x + y) `mod` m
prop_addMod_mod :: Int -> Int -> Property
prop_addMod_mod x y = forAll genMod $ \m ->
    addMod x y m `mod` m == (x + y) `mod` m

-- addMod x 0 m == x `mod` m
prop_addMod_neutral :: Int -> Property
prop_addMod_neutral x = forAll genMod $ \m ->
    addMod x 0 m == x `mod` m

-- addMod x y m == addMod y x m
prop_addMod_comm :: Int -> Int -> Property
prop_addMod_comm x y = forAll genMod $ \m ->
    addMod x y m == addMod y x m

-- Переворот пустой строки даёт пустую строку.
prop_rev_empty :: Bool
prop_rev_empty = reverseWords "" == ""

-- Переворот строки с одним словом возвращает то же слово.
prop_rev_single :: Property
prop_rev_single = forAll genWord $ \w ->
    reverseWords w == w

-- Для нескольких слов порядок слов меняется на обратный.
prop_rev_order :: Property
prop_rev_order = forAll genPhrase $ \s ->
    words (reverseWords s) == reverse (words s)

-- Двойное применение возвращает исходную строку.
prop_rev_involution :: Property
prop_rev_involution = forAll genText $ \s ->
    reverseWords (reverseWords s) == s

main :: IO ()
main = do
    putStrLn "addMod -- приведение по модулю:"
    quickCheck (withMaxSuccess 5 (verbose prop_addMod_mod))
    putStrLn "addMod -- нейтральный элемент:"
    quickCheck (withMaxSuccess 5 (verbose prop_addMod_neutral))
    putStrLn "addMod -- коммутативность:"
    quickCheck (withMaxSuccess 5 (verbose prop_addMod_comm))
    putStrLn "reverseWords -- пустая строка:"
    quickCheck prop_rev_empty
    putStrLn "reverseWords -- одно слово:"
    quickCheck (withMaxSuccess 5 (verbose prop_rev_single))
    putStrLn "reverseWords -- смена порядка слов:"
    quickCheck (withMaxSuccess 5 (verbose prop_rev_order))
    putStrLn "reverseWords -- двойное применение:"
    quickCheck (withMaxSuccess 5 (verbose prop_rev_involution))
