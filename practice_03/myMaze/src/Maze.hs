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

import qualified Data.ByteString.Char8 as BC
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

isFinish:: Room -> Bool
isFinish (Room (Start _) _) = False
isFinish (Room (Finish _) _) = True
isFinish (Room (Node _) _) = False

findFinish:: Maze -> Maybe Room
findFinish []  = Nothing
findFinish ((Room (Finish name) neighbours) : rest) = Just(Room (Finish name) neighbours)
findFinish ((Room (Start name) neighbours) : rest)  = findFinish rest 
findFinish ((Room (Node name) neighbours) : rest)  = findFinish rest 

findStart:: Maze -> Maybe Room
findStart []  = Nothing
findStart ((Room (Start name) neighbours) : rest) = Just(Room (Start name) neighbours)
findStart ((Room (Finish name) neighbours) : rest)  = findStart rest 
findStart ((Room (Node name) neighbours) : rest)  = findStart rest 

