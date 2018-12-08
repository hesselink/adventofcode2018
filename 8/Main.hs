module Main where

import Control.Monad
import Data.Char
import Safe
import Text.ParserCombinators.ReadP

import Debug.Trace


main :: IO ()
main = do
  f <- readFile "input"
  let Just t = parseTree f
  putStrLn $ "Answer one: " ++ show (sum t)
  putStrLn $ "Answer two: " ++ show (value t)

data Tree a = Tree
  { children :: [Tree a]
  , metadata :: [a]
  } deriving Show

instance Foldable Tree where
  foldMap f = cata (\bs as -> mconcat bs `mappend` foldMap f as)

cata :: ([b] -> [a] -> b) -> Tree a -> b
cata f t = f (map (cata f) (children t)) (metadata t)

value :: Tree Int -> Int
value = cata valueF
  where
    valueF childValues mds =
      if null childValues
      then sum mds
      else sum [ atDef 0 childValues (md - 1) | md <- mds ]

parseTree :: String -> Maybe (Tree Int)
parseTree = runParser pTree

pTree :: ReadP (Tree Int)
pTree = do
  numChildren <- pNumber
  skipSpaces
  numMetadata <- pNumber
  skipSpaces
  ch <- replicateM numChildren pTree
  skipSpaces
  md <- replicateM numMetadata (pNumber <* skipSpaces)
  skipSpaces
  return $ Tree { children = ch, metadata = md }

pNumber :: ReadP Int
pNumber = read <$> munch1 isNumber

runParser :: Show a => ReadP a -> String -> Maybe a
runParser parser input = 
  case [ x | (x,"") <- readP_to_S parser input ] of
    [x] -> Just x
    []  -> traceShow "no items with empty rest" Nothing
    res -> traceShow ("multiple results: ", res) Nothing
