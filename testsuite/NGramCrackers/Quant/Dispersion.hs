module NGramCrackers.Quant.Dispersion
( mean
, variance
, standardDev
, standardError
, standardErrorVar
, covar
, zScore
, cor
) where

import Data.List

mean :: (Fractional a) => [a] -> a 
mean vals = 
    let summed   = sum vals
        len      = genericLength vals
    in  summed / len

variance :: (Fractional a, Floating a) => [a] -> a
variance vals =
    let sample   = (genericLength vals) - 1
        squares  = sum [(x - m)^2 | x <- vals, let m = mean vals] 
    in  squares / sample 

standardDev :: (Fractional a, Floating a) => [a] -> a
standardDev vals =
    let var      = variance vals 
    in  sqrt var

standardError :: (Fractional a, Floating a) => [a] -> a 
standardError vals = 
    let n          = sqrt $ genericLength vals
    in standardDev vals / n 

standardErrorVar :: (Fractional a, Floating a) => [a] -> a
standardErrorVar vals =
   let var = variance vals 
       n   = genericLength vals
   in  var * sqrt (2.0 / (n - 1.0))

covar :: (Fractional a, Floating a) => [a] -> [a] -> a
covar x y =
    let sample =(genericLength x) - 1
        mx     = mean x
        my     = mean y
        cps    = [(xv - mx)*(yv - my) | (xv, yv) <- zip x y]
        scp    = sum cps
    in  scp / sample

zScore :: (Fractional a, Floating a) => [a] -> [a]
zScore vals =
    let xmd = [(x-mx) | x <- vals, let mx = mean vals]
    in [y / sdx | y <- xmd, let sdx = standardDev vals]
    
cor :: (Fractional a, Floating a) => [a] -> [a] -> a
cor x y =
    let xzs = zScore x
        yzs = zScore y
    in sum [(zx * zy) / sample | (zx, zy) <- zip xzs yzs, let sample = (genericLength x) - 1]
 
