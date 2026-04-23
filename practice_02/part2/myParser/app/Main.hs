module Main (main) where

import MyTypes.MyTree()
import MyTypes.MyMaybe()
import MyTypes.MyEither()

import qualified MyParsers.MyParser as My

main :: IO ()
main = do

    putStrLn "\nMyParser"
    putStrLn "MyParser:"
    putStrLn $ show (My.runParser My.plusOrMult "12*345dsf")
    putStrLn $ show (My.runParser My.plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    -- putStrLn $ show (runParser plusOrMultParsec "12*345dsf")
    -- putStrLn $ show (runParser plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    -- putStrLn $ show (runParser plusOrMultAttoparsec "12*345dsf")
    -- putStrLn $ show (runParser plusOrMultAttoparsec "12+345dsf")