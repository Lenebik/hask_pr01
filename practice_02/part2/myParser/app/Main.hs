module Main (main) where

import MyTypes.MyTree()
import MyTypes.MyMaybe()
import MyTypes.MyEither()

import qualified MyParsers.MyParser as My
import qualified MyParsers.ParsecParser as Parsec
import qualified MyParsers.AttoparsecParser as Atto

main :: IO ()
main = do

    putStrLn "\nMyParser"
    putStrLn "MyParser:"
    putStrLn $ show (My.runParser My.plusOrMult "12*345dsf")
    putStrLn $ show (My.runParser My.plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    putStrLn $ show (Parsec.runParsecParser Parsec.plusOrMultParsec "12*345dsf")
    putStrLn $ show (Parsec.runParsecParser Parsec.plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    putStrLn $ show (Atto.runParser Atto.plusOrMultAttoparsec "12*345dsf")
    putStrLn $ show (Atto.runParser Atto.plusOrMultAttoparsec "12+345dsf")