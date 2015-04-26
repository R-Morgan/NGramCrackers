module NGramCrackers.Utilities.Tuple
( fst'
, snd'
, thrd
, compareDoubles
, compareDoublesInList
, stringifyLexemeCount
, doubleToCSV
, tripleToCSV
, combineCountMI
, compareTriples
, compareTriplesInList
) where

import qualified Data.Maybe as M (fromJust)
import qualified Data.Text  as T

(<#>) :: T.Text -> T.Text -> T.Text
(<#>) = T.append

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

thrd :: (a , b , c) -> c
thrd (_, _, x) = x

compareDoubles:: Ord b => (a, b) -> (a, b) -> (a, b)
compareDoubles xs  ys
    | max (snd xs) (snd ys) == snd xs = xs
    | max (snd xs) (snd ys) == snd ys = ys
    | otherwise = error "Fail"

compareDoublesInList :: Ord b => [(a, b)] -> (a, b)
compareDoublesInList []     = error "Empty List"
compareDoublesInList [x]    = x 
compareDoublesInList (x:xs) = compareDoubles x (compareDoublesInList xs) 

stringifyLexemeCount :: (T.Text, Int) -> T.Text
stringifyLexemeCount x = wordStr <#> lexeme <#> countStr <#> 
                          (T.pack . show . snd) x
                           where wordStr  = T.pack "Word: " 
                                 lexeme   = fst x
                                 countStr = T.pack " ----- Count: " 

doubleToCSV :: (T.Text, Int) -> T.Text
doubleToCSV x = ngram <#> commaChar <#> (T.pack . show . snd) x
                 where ngram = fst x
                       commaChar = T.singleton ','

tripleToCSV :: (T.Text, Int, Maybe Double) -> T.Text
tripleToCSV trpl = ngram <#> commaChar <#> count <#> commaChar <#> pmi where
                     ngram     = fst' trpl
                     count     = (T.pack . show . snd') trpl
                     pmi       = M.fromJust $ T.pack <$> show <$> thrd trpl
                     -- fromJust unwrapps the Maybe T.Text value. This doesn't
                     -- seem like the best way to do this.
                     commaChar = T.singleton ','

combineCountMI :: (T.Text, Int) -> (T.Text, Maybe Double) -> (T.Text, Int, Maybe Double)
combineCountMI cntTup miTup = (txt, count, pmi) where
                 txt = fst cntTup
                 count = snd cntTup
                 pmi   = snd miTup

compareTriples :: Ord c => (a, b, c) -> (a, b, c) -> (a, b, c)
compareTriples xs ys
    | max (thrd xs) (thrd ys) == thrd xs = xs
    | max (thrd xs) (thrd ys) == thrd ys = ys
    | otherwise = error "Fail"

compareTriplesInList :: Ord c => [(a, b, c)] -> (a, b, c)
compareTriplesInList []     = error "Empty List"
compareTriplesInList [x]    = x 
compareTriplesInList (x:xs) = compareTriples x (compareTriplesInList xs) 
