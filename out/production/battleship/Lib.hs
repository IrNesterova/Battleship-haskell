module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Coordinate = (Int, Int)
data Battleship = Battleship {occupy:: [Coordinate]} deriving (Eq)
data Player = Player {name :: String, ships :: [Battleship], shotFired :: [Coordinate]}

gameOver :: Player -> Player -> Bool
gameOver p1 p2 = (hasLost p1) || (hasLost p2)

hasLost :: Player -> Bool
hasLost (Player name ships _) = ships == []
