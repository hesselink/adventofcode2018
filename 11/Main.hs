module Main where

import Data.Ord
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as Generic

main :: IO ()
main = do
  let powers = Vector.generate 300 $ \x ->
                 Vector.generate 300 $ \y ->
                   power 7511 (x + 1, y + 1)
      squarePowers = Vector.generate 298 $ \x ->
                       Vector.generate 298 $ \y ->
                         squarePower powers (x, y)
      indexOfMax = maximumIndex squarePowers
  putStrLn $ "Answer one: " ++ show indexOfMax

type Point = (Int, Int)

power :: Int -> Point -> Int
power serial (x, y) =
  let rid = x + 10
  in hundredsDigit ((rid * y + serial) * rid) - 5

squarePower :: Vector (Vector Int) -> Point -> Int
squarePower powers (tlx, tly) = sum [ powers ! x ! y | x <- [tlx..tlx+2], y <- [tly..tly+2] ]

maximumIndex :: Ord a => Vector (Vector a) -> (Int, Int)
maximumIndex xss = f $ maxIndexBy (comparing snd) (Vector.map (maxIndexBy compare) xss)
  where
    f (x, (y, _)) = (x + 1, y + 1)

hundredsDigit :: Int -> Int
hundredsDigit n = (n `div` 100) `mod` 10

-- Copied from Data.Vector.Generic to get access to the `a` in the
-- result as well
maxIndexBy :: Generic.Vector v a => (a -> a -> Ordering) -> v a -> (Int, a)
{-# INLINE maxIndexBy #-}
maxIndexBy cmpr = Bundle.foldl1' imax . Bundle.indexed . Generic.stream
  where
    imax (i,x) (j,y) = i `seq` j `seq`
                       case cmpr x y of
                         LT -> (j,y)
                         _  -> (i,x)
