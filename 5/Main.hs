module Main where

import Data.Char

main :: IO ()
main = do
  f <- readFile "input"
  let polymer = map ord . init $ f -- Init to drop \n
  putStrLn $ "Answer one: " ++ show (length . react $ polymer)
  let polymers = [ removeUnit u polymer | u <- map ord ['A'..'Z'] ]
      reacted = map react polymers
  putStrLn $ "Answer two: " ++ show (minimum . map length $ reacted)

type Polymer = [Unit]
type Unit = Int

react :: Polymer -> Polymer
react s =
  let s' = reactOnce s
  in if s' == s
     then s
     else react s'

reactOnce :: Polymer -> Polymer
reactOnce (x:x':xs) | sameTypeOppositePolarity x x' = reactOnce xs
                    | otherwise                     = x : reactOnce (x':xs)
reactOnce xs = xs

sameTypeOppositePolarity :: Unit -> Unit -> Bool
sameTypeOppositePolarity c1 c2 = abs (c1 - c2) == 32 -- toLower c1 == toLower c2 && c1 /= c2

removeUnit :: Unit -> Polymer -> Polymer
removeUnit u = filter (\u' -> u' /= u && u' /= u+32)
