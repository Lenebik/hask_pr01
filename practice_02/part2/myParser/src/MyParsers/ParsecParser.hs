module MyParsers.ParsecParser (
    Parser, runParsecParser, digitP, multiplicationP, digitsP, 
    finalMultP, finalPlusP, plusOrMultP
) where

import Text.Parsec.String (Parser)
import Text.Parsec (digit, char, try, (<|>), parse)
import Data.Char (digitToInt)
import MyTypes.MyMaybe (MyMaybe(MyJust, MyNothing))

digitP :: Parser Int
digitP = digitToInt <$> digit

multiplicationP :: Parser Int
multiplicationP = (*) <$> digitP <* char '*' <*> digitP

digitsP :: Parser Int
digitsP = foldl (\acc d -> acc * 10 + d) 0 <$> many1 digitP
  where
    many1 p = (:) <$> p <*> many p
    many p = many1 p <|> pure []

finalMultP :: Parser Int
finalMultP = (*) <$> digitsP <* char '*' <*> digitsP

finalPlusP :: Parser Int
finalPlusP = (+) <$> digitsP <* char '+' <*> digitsP

plusOrMultP :: Parser Int
plusOrMultP = try finalMultP <|> finalPlusP

runParsecParser :: Parser Int -> String -> MyMaybe (String, Int)
runParsecParser p input =
    case parse p "" input of
        Left _ -> MyNothing
        Right result -> MyJust ("", result)