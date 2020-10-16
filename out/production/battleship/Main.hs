module Main where

import Lib

main :: IO();
main = print(head' [1,2,3,4])

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Beth"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- x:xd