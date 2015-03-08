module NGramCrackers.TupleManipulation
(
  thrd
, compareDoubles
, compareDoublesInList
, stringifyLexemeCount
, doubleToCSV
, compareTriples
, compareTriplesInList
) where

import qualified Data.Text as T

thrd :: (a , b , c) -> c
thrd (_, _, x) = x

compareDoubles:: Ord b => (a, b) -> (a, b) -> (a, b)
compareDoubles xs  ys
    | max (snd xs) (snd ys) == (snd xs) = xs
    | max (snd xs) (snd ys) == (snd ys) = ys
    | otherwise = error "Fail"

compareDoublesInList :: Ord b => [(a, b)] -> (a, b)
compareDoublesInList []     = error "Empty List"
compareDoublesInList [x]    = x 
compareDoublesInList (x:xs) = compareDoubles x (compareDoublesInList xs) 

stringifyLexemeCount :: (T.Text, Int) -> T.Text
stringifyLexemeCount x = T.pack "Word: " `T.append`  fst x `T.append` T.pack " ----- Count: " `T.append` (T.pack . show . snd) x 

doubleToCSV :: (T.Text, Int) -> T.Text
doubleToCSV x = fst x `T.append` T.singleton ',' `T.append` (T.pack . show . snd) x

compareTriples :: Ord c => (a, b, c) -> (a, b, c) -> (a, b, c)
compareTriples xs ys
    | max (thrd xs) (thrd ys) == (thrd xs) = xs
    | max (thrd xs) (thrd ys) == (thrd ys) = ys
    | otherwise = error "Fail"


compareTriplesInList :: Ord c => [(a, b, c)] -> (a, b, c)
compareTriplesInList []     = error "Empty List"
compareTriplesInList [x]    = x 
compareTriplesInList (x:xs) = compareTriples x (compareTriplesInList xs) 

--orderLexemeProfile :: Ord a => [(a, b)] -> [(a, b)]

