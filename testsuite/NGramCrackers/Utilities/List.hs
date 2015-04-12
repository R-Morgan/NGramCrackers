module NGramCrackers.Utilities.List
( getNSeqFromList
, intToList
) where

import Data.List

{-| Function creates a list of lists of n length -}
getNSeqFromList :: Int -> [a] -> [[a]]
getNSeqFromList n [] = []
getNSeqFromList n (x:xs) 
    | n < 0              = error "n must be a positive integer less than 7"
    | n > 7              = error "n must be a positive integer less than 7"
    | length (x:xs) >= n = take n (x:xs) : getNSeqFromList n xs
    | otherwise = []

intToList :: Integer -> [Int]
intToList i = map read $ words (intersperse ' ' $ show i) :: [Int]
