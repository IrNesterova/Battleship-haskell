module Types.Game where

--координата, хз зачем, но пусть будет
type Coordinate = (Int, Int)
--для того, чтобы показать что это
data Block = Water | ShipPart | Swell | Hit | Miss deriving (Show, Eq)
--тип корабля для красоты
data ShipType = Destroyer | Submarine | Cruiser | Battleship | Carrier deriving (Show,Eq)

--тип корабля и сколько клеток занимает
data Ship = Ship{occupy::[Coordinate], shipType :: ShipType} deriving (Show, Eq)
--поле
data Board = Board {rows :: [[Block]]} deriving (Show)
--игрок с именем, кораблями, выстрелями и полем
data Player = Player{name::String, ships::[Ship], shotsFired:: [Coordinate], board :: Board} deriving (Show)

-- не уверена, статус игры?
data GameState = GameState{currentPlayer:: Int, player1 :: Player, player2 :: Player}

data Game = Game {board1 :: Board, board2 :: Board}

--конец игры
gameOver :: Player -> Player->Bool
gameOver p1 p2 = hasLost p1 || hasLost p2

--проверяем, когда проиграл - если кораблей нет
hasLost :: Player->Bool
hasLost (Player name ships _ _) = ships == []

--пустое поле, просто везде вода
emptyBoard :: Board
emptyBoard = Board (replicate 10 (replicate 10 Water))

--размеры корабля
shipSize :: ShipType -> Int
shipSize Destroyer = 1
shipSize Submarine = 2
shipSize Cruiser = 3
shipSize Battleship = 4
shipSize Carrier = 5

--выстрел по координате
shoot :: Board -> Coordinate -> Board
shoot b (x,y) = setBlock b (x,y)(shoot'' (getBlock b (x,y)))
  where shoot'' :: Block -> Block
        shoot'' ShipPart = Hit
        shoot'' Hit = Hit
        shoot'' b = Miss

--добавить блоки
addBlocks:: Board -> [Coordinate] -> Block -> Board
addBlocks board [] _ = board
addBlocks board (x:xs) block | isValid x = addBlocks(setBlock board x block) xs block
                             | otherwise = addBlocks board x block
--тип блока
getBlock :: Block -> Coordinate -> Block
getBlock (Board b) (x,y) = (b !! y) !! x

--задает блок
setBlock:: Board -> Coordinate -> Block -> Board
setBlock (Board b)(x,y) block = Board (take y b ++ [setBlock' (b !! y) x block] ++ drop (y+1) b)
  where setBlock' :: [Block] -> Int -> Block -> [Block]
        setBlock' b x block = take x b ++ [block] ++ drop (x+1) b

--очистить корабль
cleanShip :: [Ship] -> [Ship]
cleanShip ships = Prelude.filter(/= Ship[]) ships

--проверка попала ли
isHit :: Board -> Coordinate -> Bool
isHit b (x,y) | getBlock b (x,y) == Hit = True
              | otherwise = False

--проверка, а стреляли ли мы по этой клетке
isBlockShotAt :: Board -> Coordinate -> Bool
isBlockShotAt b (x,y) | block == Hit || block == Miss = True
                      | otherwise = False
             where block = getBlock b (x,y)

--лист попаданий
listHits :: Board -> [Coordinate]
listHits b = listCoordsOfBlocks b Hit

--лист неизвестности, на самом деле просто пустые клетки
listUnexplored :: Board -> [Coordinate]
listUnexplored b = listCoordsOfBlocks b Water
        ++ listCoordsOfBlocks b ShipPart
        ++ listCoordsOfBlocks b Swell

--проверить валидность координат
isValid :: Coordinate -> Bool
isValid (x,y) | x > 0 && x<=9 && y > 0 && y<=9 = True
              | otherwise = False

listCoordsOfBlocks :: Board -> Block -> [Coordinate]
listCoordsOfBlocks (Board board) = listCoor' 0 board
  where listCoor' :: Int -> [[Block]] -> Block -> [Coordinate]
        listCoor' _ [] b = []
        listCoor' y (row:rows) b = listCoor'' 0 y row b ++ listCoor' (y+1) rows b
        listCoor'' :: Int -> Int -> [Block] -> Block -> [Coordinate]
        listCoor'' _ _ [] b = []
        listCoor'' x y (block:row) b | block ==b = (x,y) : listCoor'' (x+1) y row b
                                     | otherwise = listCoor'' (x+1) y row b

listNeighbours :: Board -> Coordinate -> [(Coordinate, Block)]
listNeighbours b (x,y) = getBlockIfValid b (x+1, y)
                       ++ getBlockIfValid b (x-1, y)
                       ++ getBlockifValid b (x, y+1)
                       ++ getBlockIfValid b (x, y-1)
--валидный блок
getBlockIfValid :: Board -> Coordinate -> [(Coordinate, Block)]
getBlockIfValid b (x,y) | isValid (x,y) = [((x,y), getBlock b (x,y))]
                        | otherwise = []



