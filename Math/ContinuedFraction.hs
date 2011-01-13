{-# LANGUAGE BangPatterns #-}

module Math.ContinuedFraction (CF
                              ,fromDouble
                              ,toDouble
                              ,fromDoubleP
                              ,takeCF
                              ) 
where

import qualified Data.Vector as V

-- |Continued fraction datatype, storing coefficients in a 'Data.Vector'
newtype CF = CF { unCF :: V.Vector Integer }

instance Show CF where
    show = show . unCF

restLimit = 2**128

splitNum x = let y = floor x :: Integer
                 (!a, !b) = (y, 1 / (x - (fromInteger y :: Double))) 
             in (a,b)

fracStep x = if x > restLimit then Nothing else Just . splitNum $ x

-- |Computes the continued fraction of the given real number
fromDouble :: Double -> CF
fromDouble = CF . V.unfoldr fracStep

-- |Computes the first 'n' coefficients of the continued fraction associated to the real number
-- P stands for 'Partial'
fromDoubleP :: Int -> Double -> CF
fromDoubleP n = CF . V.unfoldrN n fracStep

-- |Computes the real number corresponding to the given continuous fraction
toDouble :: CF -> Double
toDouble = V.foldr (\y x -> let z = x + 1 / (fromInteger y :: Double) in z `seq` z) 0 . unCF

-- |Returns a 'Data.Vector' with the first 'n' coefficients of the continued fraction
takeCF :: Int -> CF -> V.Vector Integer
takeCF n = V.take n . unCF