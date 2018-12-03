module Main where

import Text.ParserCombinators.ReadP
import Data.Map (Map)
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input"
  let claims = mapMaybe parseClaim . lines $ f
      counts = Map.unionsWith (+) . map claimedCoords $ claims
      res    = length . filter (> 1) . map snd . Map.toList $ counts
  putStrLn $ "Answer one: " ++ show res
  let res2 = map id_
           . filter (\claim -> all (\c -> maybe True (< 2) $ Map.lookup c counts)
                             . map fst
                             . Map.toList
                             . claimedCoords
                             $ claim)
           $ claims
  putStrLn $ "Answer two: " ++ show res2

data Claim = Claim
  { id_    :: Integer
  , xPos   :: Integer -- inches from left edge
  , yPos   :: Integer -- inches from top edge
  , width  :: Integer
  , height :: Integer
  } deriving Show

claimedCoords :: Claim -> Map (Integer, Integer) Integer
claimedCoords claim =
  Map.fromList [ ((x, y), 1) | x <- [xPos claim .. xPos claim + width  claim - 1]
                             , y <- [yPos claim .. yPos claim + height claim - 1]
               ]

parseClaim :: String -> Maybe Claim
parseClaim = runParser parser
  where
    parser :: ReadP Claim
    parser = Claim
          <$  char '#'
          <*> number
          <*  skipSpaces
          <*  char '@'
          <*  skipSpaces
          <*> number
          <*  char ','
          <*> number
          <*  char ':'
          <*  skipSpaces
          <*> number
          <*  char 'x'
          <*> number

number :: ReadP Integer
number = read <$> munch1 isNumber

runParser :: ReadP a -> String -> Maybe a
runParser parser input = 
  case [ x | (x,"") <- readP_to_S parser input ] of
    [x] -> Just x
    []  -> Nothing
    _   -> Nothing
