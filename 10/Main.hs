module Main where

import Data.Function
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP
import Debug.Trace
import Data.Maybe
import Data.Char
import Control.Arrow

main :: IO ()
main = do
  f <- readFile "input"
  let lights = mapMaybe parseLine . lines $ f
      steps = iterate step lights
      withSizes = map (bounds . map fst &&& id) steps
      withMinSize = fromJust $ localMinOn (size . fst . fst) (zip withSizes [0..])
  putStrLn $ "Answer one: \n" ++ (plot . snd . fst $ withMinSize)
  putStrLn $ "Answer two: " ++ show (snd withMinSize)

type Point = (Int, Int)
type Light = (Point, Point) -- position, velocity

size :: (Point, Point) -> Int
size ((minX, minY), (maxX, maxY)) = (maxX - minX) * (maxY - minY)

bounds :: [Point] -> (Point, Point)
bounds cs =
  let (xs, ys) = unzip cs
  in ((minimum xs, minimum ys), (maximum xs, maximum ys))

plot :: [Light] -> String
plot lights
  = intercalate "\n"
  . plotLines minX minY maxX maxY
  . traceShowId
  . groupBy ((==) `on` snd)
  . sortBy (comparing snd <> comparing fst)
  $ ps
  where
    ps = map fst lights
    ((minX, minY), (maxX, maxY)) = bounds ps

plotLines :: Int -> Int -> Int -> Int -> [[Point]] -> [String]
plotLines minX minY maxX _    ls = go minY ls
  where
    go n v@((l@((_, y):_)):rs) | n == y    = plotLine minX maxX l : go (n + 1) rs
                               | otherwise = replicate (maxX - minX) ' ' : go (n + 1) v
    go _ _ = []

plotLine :: Int -> Int -> [Point] -> String
plotLine minX maxX ps = go minX ps
  where
    go n _ | n > maxX = ""
    go n v@((x,_):xs) | n > x     = go n xs
                      | n == x    = '#' : go (n + 1) xs
                      | otherwise = '.' : go (n + 1) v
    go n [] = replicate (maxX - n + 1) '.'

step :: [Light] -> [Light]
step = map move

move :: Light -> Light
move ((x, y), v@(vx, vy)) = ((x + vx, y + vy), v)

parseLine :: String -> Maybe Light
parseLine = runParser pLine

pLine :: ReadP Light
pLine = (,) <$ string "position=" <*> pPoint <* string " velocity=" <*> pPoint

pPoint :: ReadP Point
pPoint = (,) <$ char '<' <* skipSpaces <*> pNumber <* string "," <* skipSpaces <*> pNumber <* char '>'

pNumber :: ReadP Int
pNumber = option id (negate <$ char '-') <*> (read <$> munch1 isNumber)

runParser :: Show a => ReadP a -> String -> Maybe a
runParser parser input = 
  case [ x | (x,"") <- readP_to_S parser input ] of
    [x] -> Just x
    []  -> traceShow "no items with empty rest" Nothing
    res -> traceShow ("multiple results: ", res) Nothing

localMinOn :: Ord b => (a -> b) -> [a] -> Maybe a
localMinOn _ []  = Nothing
localMinOn _ [x] = Just x
localMinOn f (x:y:xs) | f y < f x     = localMinOn f (y:xs)
                      | otherwise     = Just x
