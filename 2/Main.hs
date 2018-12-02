module Main where

import Control.Arrow
import Data.List
import Data.Maybe

main :: IO ()
main = do
  f <- readFile "input"
  let ws = lines f
      twoThrees = map hasTwoAndThree ws
      checksum = uncurry (*)
               . (sum *** sum)
               . unzip
               . map (\(two, three) -> (if two then 1 else 0, if three then 1 else 0))
               $ twoThrees
  putStrLn $ "Answer one: " ++ show checksum
  let rs = [ fromJust diff | w1 <- ws, w2 <- ws, w1 /= w2, let diff = diffOne w1 w2, isJust diff]
  putStrLn $ "Answer two: " ++ show rs

hasTwoAndThree :: Ord a => [a] -> (Bool, Bool)
hasTwoAndThree
  = (\xs -> (any (== 2) xs, any (== 3) xs))
  . map length
  . group
  . sort

-- Return matching items if the lists differ in max 1 position
diffOne :: Eq a => [a] -> [a] -> Maybe [a]
diffOne = go False []
  where
    go _       res []     []     = Just (reverse res)
    go _       _   []     _      = error "Non matching input lengths"
    go _       _   _      []     = error "Non matching input lengths"
    go dropped res (x:xs) (y:ys) | x == y      = go dropped (x:res) xs ys
                                 | not dropped = go True res xs ys
                                 | otherwise   = Nothing
