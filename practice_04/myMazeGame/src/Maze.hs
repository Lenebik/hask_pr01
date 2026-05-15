module Maze (RoomType(..), Room(..), Maze, getName, getNeighbours, isFinish,
             findRoom, findStart, isInPath, loadMaze, playMaze) where

-- Реализация из practice_03
import Control.Monad.Reader as R
import Control.Monad.Writer as W
import MyStateT as M

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

-- Реализация парсера текстовых файлов
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

-- реализация игровой логики ; доб в practice_04
type Game a = ReaderT Maze (WriterT [String] (MyStateT String IO)) a

io :: IO a -> Game a
io = R.lift . W.lift . M.lift

logPrint :: String -> Game ()
logPrint msg = R.lift (tell [msg]) >> io (putStrLn msg)

getCRoom :: Game String
getCRoom = R.lift $ W.lift $ myState $ \s -> (s, s)

setCRoom :: String -> Game ()
setCRoom name = R.lift $ W.lift $ myState $ \_ -> ((), name)

runGame :: Maze -> String -> Game a -> IO (a, [String], String)
runGame maze startRoom game = do
    (result, fin) <- runMyStateT (runWriterT (runReaderT game maze)) startRoom
    let (a, log) = result
    return (a, log, fin)

-- Развертывание типов в runGame

-- 1) снимаем ReaderT: runReaderT :: ReaderT r m a -> r -> m a
-- runReaderT game maze :: WriterT [String] (MyStateT String IO) a (передали maze, получили внутреннюю монаду)
-- 2) снимаем WriterT: runWriterT :: WriterT w m a -> m (a, w)
-- runWriterT (runReaderT game maze) :: MyStateT String IO (a, [String])
-- writer возвращает свой лог в виде пары (результат, лог) (a, log)
-- 3) снимаем MyStateT: runMyStateT :: MyStateT s m a -> s -> m (a, s)
-- runMyStateT (runWriterT (runReaderT game maze)) startRoom :: IO ((a, [String]), String)
-- state возвращает финальное состояние парой (результат игры, финальное состояние) (result, fin), где результат - пара от writer
-- получаем кортеж из трех значений (a, log, fin)

gameProc :: Game ()
gameProc = do
    maze    <- ask
    current <- getCRoom
    case findRoom maze current of
        Nothing   -> logPrint "Комната не найдена!"
        Just room ->
            if isFinish room
                then logPrint $ "Вы достигли комнаты " ++ current ++ "!"
                else do
                    logPrint $ "\nКомната: " ++ current
                    logPrint $ "Можно пойти: " ++ unwords (getNeighbours room)
                    io $ putStr "Введите следующую комнату ('q' — выход): "
                    input <- io getLine
                    if input == "q"
                        then logPrint "Выход."
                        else if isInPath (getNeighbours room) input
                            then setCRoom input >> gameProc
                            else logPrint ("Нет пути до " ++ input) >> gameProc

playMaze :: FilePath -> IO ()
playMaze path = do
    maze <- loadMaze path
    case findStart maze of
        Nothing    -> putStrLn "Нет старта!"
        Just start -> do
            let startName = getName start
            (_, logs, _) <- runGame maze startName gameProc
            putStrLn "\nЛог:"
            mapM_ putStrLn logs
