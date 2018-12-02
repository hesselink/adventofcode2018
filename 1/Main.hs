module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
  f <- readFile "input"
  let ls = lines f
      ns = map readOne ls
      s  = sum ns
  print $ "Answer one: " ++ show s
  let infNs = cycle ns
      sums = scanl (+) 0 infNs
      r = firstDup sums
  print $ "Answer two: " ++ show r

readOne :: String -> Integer
readOne ('+':n) = read n
readOne n = read n

firstDup :: Ord a => [a] -> a
firstDup = go Set.empty
  where
    go seen (x:xs) | x `Set.member` seen = x
                   | otherwise = go (Set.insert x seen) xs

