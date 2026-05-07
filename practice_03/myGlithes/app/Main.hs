module Main (main) where

import System.Environment
import qualified Data.ByteString.Char8 as BC
import Control.Monad (foldM)
import Glitches (randomReplaceByte, randomSortSection)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName

    glitched1 <- randomReplaceByte imageFile
    let glitchedFileName1 = mconcat ["glitched_v1_", fileName]
    BC.writeFile glitchedFileName1 glitched1
    putStrLn $ "Готово! " ++ glitchedFileName1

    glitched2 <- randomSortSection imageFile
    let glitchedFileName2 = mconcat ["glitched_v2_", fileName]
    BC.writeFile glitchedFileName2 glitched2
    putStrLn $ "Готово! " ++ glitchedFileName2

    glitched3 <- foldM (\bytes func -> func bytes) imageFile
                [ randomReplaceByte , randomSortSection
                , randomReplaceByte , randomSortSection
                , randomReplaceByte]
    let glitchedFileName3 = mconcat ["glitched_v3_", fileName]
    BC.writeFile glitchedFileName3 glitched3
    putStrLn $ "Готово! " ++ glitchedFileName3