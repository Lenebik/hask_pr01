module MyParsers.AttoparsecParser (
    Parser, runParser, digitA, multiplicationA, digitsA, 
    finalMultA, finalPlusA, plusOrMultAttoparsec
) where

import Data.Attoparsec.Text
import Control.Applicative (some, (<|>))
import MyTypes.MyMaybe (MyMaybe(..))
import Data.Char (digitToInt)
import qualified Data.Text as T

digitA :: Parser Int
digitA = digitToInt <$> digit

multiplicationA :: Parser Int 
multiplicationA = (*) <$> digitA <* char '*' <*> digitA 

digitsA :: Parser Int
digitsA = Prelude.foldl (\x y -> x * 10 + y) 0 <$> some digitA

finalMultA :: Parser Int
finalMultA = (*) <$> digitsA <* char '*' <*> digitsA

finalPlusA :: Parser Int
finalPlusA = (+) <$> digitsA <* char '+' <*> digitsA

plusOrMultAttoparsec :: Parser Int
plusOrMultAttoparsec = try finalMultA <|> finalPlusA

runParser :: Parser Int -> String -> MyMaybe (String, Int)
runParser p input = case parseOnly ((,) <$> p <*> takeText) (T.pack input) of
    Left _ -> MyNothing
    Right (result, rest) -> MyJust (T.unpack rest, result)