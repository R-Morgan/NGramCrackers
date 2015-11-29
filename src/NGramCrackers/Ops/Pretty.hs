{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.Ops.Pretty
( formatOutput
, ngramLister
, ngRecFormatter
, printMaybe
, statsFormatter
) where

import qualified Data.Text.IO as TIO
import qualified System.IO    as SIO

import qualified Data.Text as T

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
printMaybe Nothing = ""
printMaybe (Just txt) = txt

statsFormatter :: DocCol T.Text -> T.Text
statsFormatter stream = (mean <#> sd <#> var) where
                           mean = "Mean: " <#> (ps . meanSentsPerParagraph) stream    <#> " "
                           sd   = "SD: "  <#> (ps . sdSentsPerParagraph)   stream     <#> " "
                           var  = "Variance: " <#> (ps . varSentsPerParagraph) stream <#> " "
                           ps   = T.pack . show
