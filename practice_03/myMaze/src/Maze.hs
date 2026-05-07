-------------------------------------
-- Практическое задание 3. Часть 2 --
-------------------------------------

{-

Реализуйте поиск маршрута прохождения лабиринта с использованием монады RWS
    - карта лабиринта является внутренней переменной
    - каждая комната имеет уникальное текстовое название
    - маршрут начинается в комнате "старт"
    - маршрут заканчивается в комнате "финиш"
    - реализуйте поиск маршрута от "старта" до "финиша"
    - все перемещения логируются и выводятся на экран после завершения поиска
    - в реализации не использовать do нотацию

-}

module Maze (
    RoomType(..),
    Room(..),
    Maze,
    solveMaze,
    getName,
    getNeighbours,
    findRoom,
    findStart,
    findFinish,
) where

import qualified Control.Monad.RWS as RWS

data RoomType = Start String | Finish String | Node String deriving (Eq)

data Room = Room RoomType [String] deriving (Eq) -- где [String] - список соседей

type Maze = [Room]

getName :: Room -> String
getName (Room (Start name) _) = name
getName (Room (Finish name) _) = name
getName (Room (Node name) _) = name

getNeighbours :: Room -> [String]
getNeighbours (Room _ x) = x

isFinish (Room (Finish _) _) = True
isFinish _ = False

findFinish :: Maze -> Maybe Room
findFinish [] = Nothing
findFinish ((Room (Finish name) neighbours) : _) = Just (Room (Finish name) neighbours)
findFinish ((Room (Start _) _) : rest) = findFinish rest
findFinish ((Room (Node _) _) : rest) = findFinish rest

findStart :: Maze -> Maybe Room
findStart [] = Nothing
findStart ((Room (Start name) neighbours) : _) = Just (Room (Start name) neighbours)
findStart ((Room (Finish _) _) : rest) = findStart rest
findStart ((Room (Node _) _) : rest) = findStart rest

findRoom :: Maze -> String -> Maybe Room -- поиск комнаты по имени
findRoom [] _ = Nothing
findRoom (room : rest) seek 
    | getName room == seek = Just room
    | otherwise = findRoom rest seek

isInPath :: [String] -> String -> Bool -- является ли комната частью маршрута
isInPath [] _ = False
isInPath (n : rest) seek | n == seek = True
                         | otherwise = isInPath rest seek

getUnvisited :: [String] -> [String] -> [String]
getUnvisited [] _ = []
getUnvisited (n : ns) path | isInPath path n = getUnvisited ns path
                           | otherwise = (n : getUnvisited ns path)

solveMaze :: RWS.RWS Maze [String] [String] (Maybe [String])
solveMaze =
    RWS.ask >>= \maze ->
    case (findStart maze, findFinish maze) of
        (Nothing, _) ->
            RWS.tell ["Error: Start room not found"] >>
            return Nothing
        (_, Nothing) ->
            RWS.tell ["Error: Finish room not found"] >>
            return Nothing
        (Just startRoom, Just finishRoom) ->
            let startName = getName startRoom
                finishName = getName finishRoom
            in RWS.put [startName] >>
               RWS.tell ["Start at room: " ++ startName] >>
               RWS.tell ["Finish at room: " ++ finishName] >>
               searchPath maze finishName

searchPath :: Maze -> String -> RWS.RWS Maze [String] [String] (Maybe [String])
searchPath maze finishName =
    RWS.get >>= \path ->
    case path of
        [] -> return Nothing
        (currentName : _) ->
            case findRoom maze currentName of
                Nothing ->
                    RWS.tell ["Error: Room " ++ currentName ++ " not found in maze"] >>
                    return Nothing
                Just currentRoom ->
                    if isFinish currentRoom
                        then RWS.tell ["Finish: Reached room " ++ finishName] >>
                             return (Just (reverse path))
                        else
                            let unvisited = getUnvisited (getNeighbours currentRoom) path
                            in case unvisited of
                                [] ->
                                    RWS.tell ["Dead end: Room " ++ currentName ++ " has no unvisited neighbours"] >>
                                    RWS.tell ["Failure: Cannot reach finish from current path"] >>
                                    return Nothing
                                (nextRoom : _) ->
                                    RWS.tell ["Move: " ++ currentName ++ " -> " ++ nextRoom] >>
                                    RWS.put (nextRoom : path) >>
                                    searchPath maze finishName
{-
schoolMaze :: Maze
schoolMaze =
    [ Room (Start "Entrance") ["Hall"]
    , Room (Node "Hall") ["Entrance", "Mathematics", "Physical Education"]
    , Room (Node "Mathematics") ["Hall", "Computer Science"]
    , Room (Node "Computer Science") ["Mathematics", "Principal's Office"]
    , Room (Finish "Principal's Office") ["Computer Science"]
    , Room (Node "Physical Education") ["Hall"]
    ]

castleMaze :: Maze
castleMaze =
    [ Room (Start "Gate") ["Courtyard"]
    , Room (Node "Courtyard") ["Gate", "Kitchen", "Tower"]
    , Room (Node "Kitchen") ["Courtyard", "Treasure Room"]
    , Room (Finish "Treasure Room") ["Kitchen"]
    , Room (Node "Tower") ["Courtyard"]
    ]

ghci> RWS.runRWS solveMaze schoolMaze []
(Just ["Entrance","Hall","Mathematics","Computer Science","Principal's Office"],["Principal's Office","Computer Science","Mathematics","Hall","Entrance"],["Start at room: Entrance","Finish at room: Principal's Office","Move: Entrance -> Hall","Move: Hall -> Mathematics","Move: Mathematics -> Computer Science","Move: Computer Science -> Principal's Office","Finish: Reached room Principal's Office"])
ghci> RWS.runRWS solveMaze cemeteryMaze []
(Just ["Gate","Alley","Chapel","Tomb"],["Tomb","Chapel","Alley","Gate"],["Start at room: Gate","Finish at room: Tomb","Move: Gate -> Alley","Move: Alley -> Chapel","Move: Chapel -> Tomb","Finish: Reached room Tomb"])
ghci> 
-}