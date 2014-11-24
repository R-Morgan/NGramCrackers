module ListManipulation
(
  getNSeqFromList
, intToList
) where

import Data.List

{-| Function creates a list of lists of n length -}
getNSeqFromList :: Int -> [a] -> [[a]]
getNSeqFromList n [] = []
getNSeqFromList n (x:xs) 
    | length (x:xs) >= n = take n (x:xs) : getNSeqFromList n xs
    | otherwise = []

intToList :: Integer -> [Int]
intToList i = map read $ words (intersperse ' ' $ show i) :: [Int]
