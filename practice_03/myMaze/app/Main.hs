module Main where

import qualified Control.Monad.RWS as RWS
import Maze

main :: IO ()
main = do
    putStrLn "\n School Maze"
    let (result, path, logs) = RWS.runRWS solveMaze schoolMaze []
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Path: " ++ show path
    putStrLn "Log:"
    mapM_ putStrLn logs
    
    putStrLn "\n Cemetery Maze"
    let (result2, path2, logs2) = RWS.runRWS solveMaze cemeteryMaze []
    putStrLn $ "Result: " ++ show result2
    putStrLn $ "Path: " ++ show path2
    putStrLn "Log:"
    mapM_ putStrLn logs2

schoolMaze :: Maze
schoolMaze =
    [ Room (Start "Entrance") ["Hall"]
    , Room (Node "Hall") ["Entrance", "Mathematics", "Physical Education"]
    , Room (Node "Mathematics") ["Hall", "Computer Science"]
    , Room (Node "Computer Science") ["Mathematics", "Principal's Office"]
    , Room (Finish "Principal's Office") ["Computer Science"]
    , Room (Node "Physical Education") ["Hall"]
    ]

cemeteryMaze :: Maze
cemeteryMaze =
    [ Room (Start "Gate") ["Alley"]
    , Room (Node "Alley") ["Gate", "Chapel", "Crypt"]
    , Room (Node "Chapel") ["Alley", "Tomb"]
    , Room (Finish "Tomb") ["Chapel"]
    , Room (Node "Crypt") ["Alley"]
    ]