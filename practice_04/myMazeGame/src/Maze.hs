module Maze (RoomType(..), Room(..), Maze, getName, getNeighbours, isFinish, findRoom, findStart, isInPath, loadMaze,) where

data RoomType = Start String | Finish String | Node String deriving (Eq)

data Room = Room RoomType [String] deriving (Eq)

type Maze = [Room]

getName :: Room -> String
getName (Room (Start name) _) = name
getName (Room (Finish name) _) = name
getName (Room (Node name) _) = name

getNeighbours :: Room -> [String]
getNeighbours (Room _ x) = x

isFinish :: Room -> Bool
isFinish (Room (Finish _) _) = True
isFinish _ = False

findStart :: Maze -> Maybe Room
findStart [] = Nothing
findStart ((Room (Start name) neighbours) : _) = Just (Room (Start name) neighbours)
findStart (_ : rest) = findStart rest

findRoom :: Maze -> String -> Maybe Room
findRoom [] _ = Nothing
findRoom (room : rest) seek
    | getName room == seek = Just room
    | otherwise = findRoom rest seek

isInPath :: [String] -> String -> Bool
isInPath [] _ = False
isInPath (n : rest) seek | n == seek = True
                         | otherwise = isInPath rest seek

-- Формат файла (каждая строка: имя_комнаты сосед1 сосед2 ...):
-- Пример:

--   старт зал
--   зал коридор кухня
--   коридор зал финиш
--   финиш коридор

parseRoom :: String -> Room
parseRoom line =
    let (name : neighbours) = words line
        roomType = case name of
            "старт" -> Start name
            "финиш" -> Finish name
            _       -> Node name
    in Room roomType neighbours

loadMaze :: FilePath -> IO Maze
loadMaze path = map parseRoom . filter (not . null) . lines <$> readFile path
