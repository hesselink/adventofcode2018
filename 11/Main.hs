{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.ST
import Data.List
import Data.Ord
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Mutable as Mutable

import Debug.Trace

main :: IO ()
main = do
  let powers = Vector.generate 300 $ \x ->
                 Vector.generate 300 $ \y ->
                   power 18 (x + 1, y + 1)
      {-
      rectPowers = Vector.generate 300 $ \w ->
                     Vector.generate 300 $ \h ->
                       rectanglePower powers (w, h)
                       -}
      squarePowers3 = squarePowers 3 (rectanglePowers powers)
      indexOfMax = maximumIndex squarePowers3
  print powers
  putStrLn $ "Answer one: " ++ show indexOfMax
  let res = maximumBy (comparing (snd . snd)) [ (size, traceShow size $ maximumWithIndex (squarePowers size powers)) | size <- [1..300] ]
  putStrLn $ "Answer two: " ++ show res

type Point = (Int, Int)

power :: Int -> Point -> Int
power serial (x, y) =
  let rid = x + 10
  in hundredsDigit ((rid * y + serial) * rid) - 5

squarePowers :: Int -> Vector (Vector Int) -> Vector (Vector Int)
squarePowers size rectPowers =
  Vector.generate (300 - size + 1) $ \x ->
    Vector.generate (300 - size + 1) $ \y ->
      squarePower size rectPowers (x, y)

squarePower :: Int -> Vector (Vector Int) -> Point -> Int
squarePower size rectPowers (tlx, tly) = 
  let (brx, bry) = (tlx + size - 1, tly + size - 1)
  in rectPowers ! brx ! bry
   - if tly == 0 then 0 else rectPowers ! brx ! (tly - 1)
   - if tlx == 0 then 0 else rectPowers ! (tlx - 1) ! bry
   + if tlx == 0 || tly == 0 then 0 else rectPowers ! (tlx - 1) ! (tly - 1)

rectanglePower :: Vector (Vector Int) -> Point -> Int
rectanglePower powers (w, h) = sum [ powers ! x ! y | x <- [0..w-1], y <- [0..h-1] ]

rectanglePowers :: Vector (Vector Int) -> Vector (Vector Int)
rectanglePowers powers = Vector.create creator
  where
    creator :: forall s. ST s (Mutable.MVector s (Vector Int))
    creator = do
      rs <- Mutable.new 300
      mapM_ (writeCol rs) [0..299]
      return rs
      where
        writeCol :: Mutable.MVector s (Vector Int) -> Int -> ST s ()
        writeCol rs x = do
          ps <- Mutable.new 300
          mapM_ (writePower rs ps x) [0..299]
          r <- Vector.freeze ps
          Mutable.write rs x r
        writePower cs c x y = do
          v1 <- if y == 0 then return 0 else Mutable.read c (y - 1)
          (v2, v3) <- if x == 0 then return (0, 0) else do
            cPrev <- Mutable.read cs (x - 1)
            return (cPrev ! y, if y == 0 then 0 else cPrev ! (y - 1))
          let v4 = powers ! x ! y
          Mutable.write c y (v1 + v2 - v3 + v4)

maximumIndex :: Ord a => Vector (Vector a) -> Point
maximumIndex xss = f $ maxIndexBy (comparing snd) (Vector.map (maxIndexBy compare) xss)
  where
    f (x, (y, _)) = (x + 1, y + 1)

maximumWithIndex :: Ord a => Vector (Vector a) -> (Point, a)
maximumWithIndex xss = f $ maxIndexBy (comparing snd) (Vector.map (maxIndexBy compare) xss)
  where
    f (x, (y, m)) = ((x + 1, y + 1), m)

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
