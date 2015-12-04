{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.Ops.Pretty
( 
  formatOutput
, ngramLister
, ngRecFormatter
, printMaybe
, statsFormatter
, typeTokenRatio
, doubleToCSV
, tripleToCSV
) where

import Data.List (nub)

import qualified Data.Maybe   as M (fromJust)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified System.IO    as SIO

import NGramCrackers.DataTypes
import NGramCrackers.Ops.Infixes
import NGramCrackers.Quant.Counts
import NGramCrackers.Quant.Stats
import NGramCrackers.Utilities.Tuple

formatOutput :: Foldable t => SIO.Handle -> t (NG T.Text, Int) -> IO ()
formatOutput outHandle = mapM_ (TIO.hPutStrLn outHandle . doubleToCSV)

ngramLister:: DocCol T.Text -> ([NG T.Text] -> SentColl T.Text) -> [(NG T.Text, Int)] 
ngramLister r extractor = wcMapToList $ ngramMap extractor r

ngRecFormatter :: (NG T.Text, Count) -> T.Text
ngRecFormatter (ng, count) = txt <#> "," <#> count' where txt    = printMaybe $ getNG ng
                                                          count' = (T.pack . show) count

printMaybe :: Maybe T.Text -> T.Text
-- May be useful for getting rid of the fromJusts in some code
printMaybe Nothing = ""
printMaybe (Just txt) = txt

statsFormatter :: DocCol T.Text -> T.Text
statsFormatter stream = (mean <#> sd <#> var) where
                           mean = "Mean: " <#> (ps . meanSentsPerParagraph) stream    <#> " "
                           sd   = "SD: "  <#> (ps . sdSentsPerParagraph)   stream     <#> " "
                           var  = "Variance: " <#> (ps . varSentsPerParagraph) stream <#> " "
                           ps   = T.pack . show

typeTokenRatio ::  T.Text -> T.Text
typeTokenRatio string = typeStr <#> ps types <#> tokStr <#> ps tokens <#> ttrStr
                                <#> ratio
                       where types = (fromIntegral . length . nub . T.words) string
                             tokens = (fromIntegral . length . T.words) string
                             ratio = (T.pack . show) $ types / tokens
                             typeStr = T.pack "Types: "
                             tokStr  = T.pack ", Tokens: "
                             ttrStr  = T.pack ", TTR: "
                             ps      = T.pack . show

doubleToCSV :: (NG T.Text, Int) -> T.Text
doubleToCSV x = ng <#> commaChar <#> (T.pack . show . snd) x
                 where ng = (M.fromJust . getNG . fst) x
                       commaChar = T.singleton ','

tripleToCSV :: (T.Text, Int, Maybe Double) -> T.Text
tripleToCSV trpl = ngram <#> commaChar <#> count <#> commaChar <#> pmi where
                     ngram     = fst' trpl
                     count     = (T.pack . show . snd') trpl
                     pmi       = M.fromJust $ T.pack <$> show <$> thrd trpl
                     -- fromJust unwrapps the Maybe T.Text value. This doesn't
                     -- seem like the best way to do this.
                     commaChar = T.singleton ','
