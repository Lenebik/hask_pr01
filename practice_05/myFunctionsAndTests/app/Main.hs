module Main (main) where

import Lib (addMod, reverseWords)

main :: IO ()
main = do
    putStrLn ("addMod 7 8 5 = " ++ show (addMod 7 8 5))
    putStrLn ("addMod (-3) 1 5 = " ++ show (addMod (-3) 1 5))
    putStrLn ("reverseWords \"hello dear world\" = " ++ show (reverseWords "hello dear world"))
