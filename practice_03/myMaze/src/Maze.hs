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

module Maze where

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
findFinish []  = Nothing
findFinish ((Room (Finish name) neighbours) : rest) = Just(Room (Finish name) neighbours)
findFinish ((Room (Start name) neighbours) : rest)  = findFinish rest 
findFinish ((Room (Node name) neighbours) : rest)  = findFinish rest 

findStart :: Maze -> Maybe Room
findStart []  = Nothing
findStart ((Room (Start name) neighbours) : rest) = Just(Room (Start name) neighbours)
findStart ((Room (Finish name) neighbours) : rest)  = findStart rest 
findStart ((Room (Node name) neighbours) : rest)  = findStart rest 

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
            RWS.tell ["Ошибка: Стартовая комната не найдена"] >>
            return Nothing
        (_, Nothing) ->
            RWS.tell ["Ошибка: Финишная комната не найдена"] >>
            return Nothing
        (Just startRoom, Just finishRoom) ->
            let startName = getName startRoom
                finishName = getName finishRoom
            in RWS.put [startName] >>
               RWS.tell ["Старт в комнате: " ++ startName] >>
               RWS.tell ["Финиш в комнате: " ++ finishName] >>
               searchPath maze finishName
