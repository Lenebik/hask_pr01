# Haskell Functional Programming Labs

Репозиторий содержит решения лабораторных работ 1–5 по дисциплине «Функциональное программирование». Все работы выполнены на языке Haskell.


---

## Лабораторная работа №1

### Часть 1 (`pr01_1.hs`)
- Реализация стандартных функций работы со списками:
  - `myHead`, `myTail` — отделение головы и хвоста
  - `myZip`, `myZip3` — попарное и потройное объединение
- Функции высшего порядка:
  - `myMap`, `myZipWith`, `myZipWith3`
  - `myAll`, `myAny`, `myComposition`

### Часть 2 (`pr01_2.hs`)
- Проекции кортежа: `myFST`, `mySND`, `myTHRD`
- `myZipSave` / `myUnzipSave` — zip с сохранением остатка
- Свёртки:
  - `myFoldr`, `myFoldl`, `myFoldl1`, `myFoldr1`
  - `myReverse` (через свёртку)
  - `myTakeWhile`, `mySpan` (через `myFoldr`)
- `myMaybe` — обработка возможно отсутствующего значения
- `myUnfoldr` — развёртка
- Рекурсивный тип `MyList` с `myMap`
- Типизация процесса выпекания тортов:
  - `Ingredient`, `FillingMix`, `Dough`, `CakeDoughType`, `Cake`, `Action`
  - Функции: `makeCakeMix`, `cakeDough`, `chocolateCakeDough`, `vanillaCakeDough`, `carrotCakeDough`
  - Функции выпекания: `chocolateCake`, `vanillaCake`, `carrotCake`

---

## Лабораторная работа №2

### Часть 1. Тип `MyEvolution`
- Определён тип-сумма с 10 конструкторами (этапы эволюции от LUCA до Humans)
- Вручную реализованы представители классов:
  - `Show` — преобразование в строку
  - `Read` — обратное преобразование
  - `Eq` — сравнение
  - `Enum` — порядковые номера
  - `Ord` — сравнение через `fromEnum`
  - `Bounded` — минимальное и максимальное значение
- Автоматический вывод через `deriving` для типа `MyEvolution'`

### Часть 2. Собственные типы
- **`MyMaybe`** — аналог стандартного `Maybe` с представителями:
  - `Functor`, `Applicative`, `Semigroup`, `Monoid`, `Foldable`
- **`MyEither`** — аналог стандартного `Either` с представителями:
  - `Functor`, `Foldable`, `Applicative`, `Semigroup`
- **`MyTree`** — бинарное дерево с представителями:
  - `Functor`, `Foldable`, `Applicative`

### Часть 3. Парсеры
- **Собственный парсер** (`MyParser`) на основе типа `Parser tok a = [tok] -> MyMaybe ([tok], a)`:
  - `Functor`, `Applicative`, `Alternative`
  - Базовые парсеры: `satisfy`, `char`, `lower`, `digit`
  - Комбинированные: `multiplication`, `lowers`, `digits`, `finalMult`, `finalPlus`, `plusOrMult`
- **Parsec-парсер** (`ParseParser`):
  - Аналогичные парсеры с использованием библиотеки Parsec
  - `try` для backtracking
- **Attoparsec-парсер** (`AttoparseParser`):
  - Аналогичные парсеры с использованием библиотеки Attoparsec
  - Работа с `Text` вместо `String`

---

## Лабораторная работа №3

### Часть 1. Глитч-эффекты (`myGlithes`)
- `intToChar` / `intToBC` — преобразование Int в Char/ByteString
- `replaceByte` — детерминированная замена байта
- `randomReplaceByte` — замена случайного байта
- `sortSection` — сортировка участка ByteString
- `randomSortSection` — сортировка случайного участка
- Последовательное применение глитч-функций через `foldM`

### Часть 2. Лабиринт на монаде RWS (`myMaze`)
- Типы данных: `RoomType`, `Room`, `Maze`
- Функции: `getName`, `getNeighbours`, `isFinish`, `findStart`, `findFinish`, `findRoom`, `isInPath`, `getUnvisited`
- Решатель `solveMaze` на монаде `RWS`:
  - Окружение — лабиринт
  - Состояние — пройденный путь
  - Лог — журнал перемещений
- Поиск в глубину без использования `do`-нотации
- Тестовые лабиринты: `schoolMaze`, `cemeteryMaze`

---

## Лабораторная работа №4

### Игра «Прохождение лабиринта»
- Чтение лабиринта из файла (`loadMaze`, `parseRoom`)
- Игровая логика:
  - `getCRoom` / `setCRoom` — работа с текущей комнатой
  - `logPrint` — логирование с выводом в консоль
  - `gameProc` — основной игровой цикл
  - `runGame` — терминация стека трансформеров
- Тестовый лабиринт `mazes/uni.maze`

---

## Лабораторная работа №5

### Тестируемые функции (`Lib.hs`)
- `normMod` — приведение числа к остатку по модулю
- `addMod` — сложение по модулю (намеренно неверная реализация)
- `splitTokens` — разбиение на токены (слова и пробелы)
- `clean` — удаление пустых токенов
- `reverseWords` — изменение порядка слов на обратный

### QuickCheck-тесты (`Spec.hs`)
- Генераторы: `genMod`, `genWord`, `genSpaces`, `genPhrase`, `genText`
- Свойства для `addMod`:
  - `prop_addMod_mod` — согласованность с `mod` (ошибочный)
  - `prop_addMod_neutral` — нейтральный элемент (ошибочный)
  - `prop_addMod_comm` — коммутативность (проходит)
- Свойства для `reverseWords`:
  - `prop_rev_empty` — пустая строка
  - `prop_rev_single` — одно слово
  - `prop_rev_order` — порядок слов меняется на обратный
  - `prop_rev_involution` — двойное применение возвращает исходную строку

---

## Технологии

- **Язык:** Haskell (GHC)
- **Сборка:** Stack
- **Библиотеки:**
  - `QuickCheck` — тестирование свойств
  - `Parsec` / `Attoparsec` — парсеры
  - `mtl` / `transformers` — монадные трансформеры
  - `bytestring` — работа с байтовыми строками
  - `random` — генерация случайных значений
  - `JuicyPixels` — работа с изображениями (глитчи)

---

## Требования

- GHC (рекомендуется версия, указанная в `stack.yaml`)
- Stack (https://docs.haskellstack.org)
