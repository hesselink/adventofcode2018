module Main where

import Control.Arrow
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input"
  let coords = map readCoord . lines $ f
      bs = bounds coords
      closestIds = closests coords bs
      areaById = areas closestIds
      infIds = infinites coords bs
      maxFiniteArea = snd
                    . head
                    . filter (not . (`elem` infIds) . snd)
                    . sortBy (flip $ comparing snd)
                    . Map.toList
                    $ areaById
  putStrLn $ "Answer one: " ++ show maxFiniteArea
  let allDistSums = map (id &&& distances coords) . spiral . middle $ bs
      limit = 10000
      okPoints = filter ((< limit) . snd) . takeWhile (\(p, s) -> inBounds bs p || s < limit) $ allDistSums
  putStrLn $ "Answer two: " ++ show (length okPoints)

type Coord = (Int, Int)

readCoord :: String -> Coord
readCoord s =
  let (before, after) = break (== ',') s
  in (read before, read . drop 2 $ after)

distance :: Coord -> Coord -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

-- find (min, min) and (max, max) points bounding a list of points
bounds :: [Coord] -> (Coord, Coord)
bounds cs =
  let (xs, ys) = unzip cs
  in ((minimum xs, minimum ys), (maximum xs, maximum ys))

type ItemId = Int
noItemId :: ItemId
noItemId = -1

-- For all points within the bounds, calculate the closest coord from
-- the list. The items are identified by their index in the list.
closests :: [Coord] -> (Coord, Coord) -> Map Coord ItemId
closests cs ((minX, minY), (maxX, maxY)) = Map.fromList
  [ ((x, y), id_) | x <- [minX .. maxX], y <- [minY .. maxY], let id_ = closest cs (x, y)]

-- For a single point, calculate the closest coord, by index.
closest :: [Coord] -> Coord -> ItemId
closest cs p
  = idIfUnique
  . sortBy (comparing fst)
  $ [ (d, id_) | (id_, c) <- zip [0..] cs, let d = distance c p ]
  where
    idIfUnique ((d1, id_):(d2,_):_) | d1 /= d2  = id_
                                    | otherwise = noItemId
    idIfUnique [(_, id_)] = id_
    idIfUnique [] = noItemId

areas :: Map Coord ItemId -> Map ItemId Int
areas = Map.fromList . map (head &&& length) . group . sort . map snd . Map.toList

-- Find all item ids that are going to have an infinite area.
infinites :: [Coord] -> (Coord, Coord) -> [ItemId]
infinites cs ((minX, minY), (maxX, maxY)) =
  let ps =  [ (x, y) | x <- [minX .. maxX], y <- [minY - 1, maxY + 1] ] -- Points just around the bounds
         ++ [ (x, y) | x <- [minX - 1, maxX + 1], y <- [minY - 1 .. maxY + 1 ] ]
  in  nub . map (closest cs) $ ps

middle :: (Coord, Coord) -> Coord
middle ((minX, minY), (maxX, maxY)) = ((minX + maxX) `div` 2, (minY + maxY) `div` 2)

-- coords moving left n times from start, excluding start
lefts :: Coord -> Int -> [Coord]
lefts (x, y) n = [ (x', y) | x' <- [x-1,x-2 .. x-n] ]

rights :: Coord -> Int -> [Coord]
rights (x, y) n = [ (x', y) | x' <- [x+1 .. x+n] ]

ups :: Coord -> Int -> [Coord]
ups (x, y) n = [ (x, y') | y' <- [y+1 .. y+n] ]

downs :: Coord -> Int -> [Coord]
downs (x, y) n = [ (x, y') | y' <- [y-1,y-2 .. y-n] ]

spiral :: Coord -> [Coord]
spiral start = start : go (concatMap (replicate 2) [1..]) (cycle [lefts, ups, rights, downs]) start
  where
    go (steps:ss) (dir:dirs) c = let newCs = dir c steps
                                 in newCs ++ go ss dirs (last newCs)
    go _ _ _ = error "impossible"

-- find sum of distances of first coord to all other coords
distances :: [Coord] -> Coord -> Int
distances cs c = sum [ distance c c' | c' <- cs ]

inBounds :: (Coord, Coord) -> Coord -> Bool
inBounds ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY
