{-# LANGUAGE ParallelListComp, BangPatterns #-}
module Main where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

main :: IO ()
main = do
  f <- readFile "input"
  let ws = words . head . lines $ f
      players = read (ws !! 0) :: Int
      marbles = read (ws !! 6) :: Int
  putStrLn $ "Answer one: " ++ show (game players marbles)
  putStrLn $ "Answer two: " ++ show (game players (marbles * 100)) -- Takes ~45s and 4GB mem...


game :: Int -> Int -> Int
game numPlayers numMarbles
  = maximum
  . fst
  . foldl' oneTurn (Map.empty, emptyBoard)
  $ [ (m, p) | m <- [1..numMarbles] | p <- cycle [1..numPlayers] ]
  where
    oneTurn (score, board) (m, p) =
      let (newBoard, points) = place m board
      in  (Map.insertWith (+) p points score, newBoard)

data Board = Board
  { size    :: !Int
  , current :: !Int
  , items   :: !(Seq Int)
  } deriving Show

emptyBoard :: Board
emptyBoard = Board
  { size = 1
  , current = 0
  , items = Seq.singleton 0
  }

fromList :: [Int] -> Board
fromList ns = Board
  { size = length ns
  , current = 0
  , items = Seq.fromList ns
  }

place :: Int -> Board -> (Board, Int)
place n b =
  if n `mod` 23 == 0
  then second (+n) (removeCurrent . counterClockwise 7 $ b)
  else (placeCurrent n . clockwise 2 $ b, 0)

clockwise :: Int -> Board -> Board
clockwise n b = b { current = (current b + n) `mod` (size b) }

counterClockwise :: Int -> Board -> Board
counterClockwise x b = b { current = (current b - x) `modPos` (size b) }
  where
    modPos n m | n < 0     = modPos (n + m) m
               | otherwise = n

placeCurrent :: Int -> Board -> Board
placeCurrent x b = b
  { size = size b + 1
  , items = Seq.insertAt (current b) x (items b)
  }

removeCurrent :: Board -> (Board, Int)
removeCurrent b = (b
  { size = size b - 1
  , items = Seq.deleteAt (current b) (items b)
  }, fromJust $ Seq.lookup (current b) (items b))

