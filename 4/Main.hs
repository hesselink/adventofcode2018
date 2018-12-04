#!/usr/bin/env stack
-- stack --resolver lts-12.21 script --package split --package time --package safe --package containers
module Main where

import Control.Arrow ((&&&), (***))
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Safe

main :: IO ()
main = do
  f <- readFile "input"
  let logLines = mapMaybe parseLogLine . lines $ f
      sortedLog = sortBy (comparing time) logLines
      shifts = drop 1 . split (keepDelimsL $ whenElt (isBeginShift . action)) $ sortedLog
      groupedShifts = groupBy ((==) `on` guardNumber) . sortBy (comparing guardNumber) $ shifts
      totalAsleep = map ((guardNumber . head) &&& (sum . map countAsleep) &&& mostAsleepMinute &&& id) groupedShifts
      sortedTotals = sortBy (flip $ comparing (fst . snd)) totalAsleep
      mostAsleepGuard = fst . head $ sortedTotals
      mostAsleepMinuteForGuard = fst . snd . snd . head $ sortedTotals
  putStrLn $ "Answer one: " ++ show (mostAsleepGuard * mostAsleepMinuteForGuard)
  let maxAsleep = map ((guardNumber . head) &&& maxAsleepAtMinute) groupedShifts
      sortedMaxes = mapMaybe (\(id_, mTup) -> (,) <$> pure id_ <*> mTup) . sortBy (flip $ comparing (fmap snd . snd)) $ maxAsleep
      guardWithMostAsleepMinute = fst . head $ sortedMaxes
      mostAsleepMinuteForSndGuard = fst . snd . head $ sortedMaxes
  putStrLn $ "Answer two: " ++ show (guardWithMostAsleepMinute * mostAsleepMinuteForSndGuard)

data LogLine = LogLine
  { time   :: LocalTime
  , action :: Action
  } deriving Show

data Action = BeginShift Integer | FallAsleep | WakeUp
  deriving Show

type Shift = [LogLine] -- Needs to start with BeginShift, then only FallAsleep/WakeUp pairs

isBeginShift :: Action -> Bool
isBeginShift BeginShift{} = True
isBeginShift _            = False

guardNumber :: Shift -> Integer
guardNumber (ll:_) = case action ll of
  BeginShift n -> n
  _ -> error "Shift didn't start with BeginShift"
guardNumber [] = error "Empty shift"

countAsleep :: Shift -> Integer
countAsleep [] = 0
countAsleep (ll:lls) = case action ll of
  BeginShift{} -> countAsleep lls
  FallAsleep{} -> case lls of
    (ll':lls') -> case action ll' of
      WakeUp{} -> round (realToFrac (time ll' `diffLocalTime` time ll) / 60) + countAsleep lls'
      _ -> error "Action after falling asleep wasn't waking up"
    _ -> error "No further actions after falling asleep"
  _            -> error "Woke up without falling asleep"

mostAsleepMinute :: [Shift] -> Integer
mostAsleepMinute
  = fromIntegral
  . fst
  . maximumBy (comparing snd)
  . Map.toList
  . Map.unionsWith (+)
  . map minutes

maxAsleepAtMinute :: [Shift] -> Maybe (Integer, Integer)
maxAsleepAtMinute
  = fmap (fromIntegral *** id)
  . Safe.maximumByMay (comparing snd)
  . Map.toList
  . Map.unionsWith (+)
  . map minutes

minutes :: Shift -> Map Int Integer
minutes sh = go (drop 1 sh) -- The begin shift
  where
    go (LogLine { time = t1, action = FallAsleep } : LogLine { time = t2, action = WakeUp } : lls) =
      Map.fromList [ (m, 1) | m <- [ getMinutes t1 .. getMinutes t2 - 1 ] ] `Map.union` go lls
    go [] = Map.empty
    go _ = error "Incorrect action order for shift"
    getMinutes = todMin . localTimeOfDay

parseLogLine :: String -> Maybe LogLine
parseLogLine = runParser parser
  where
    parser :: ReadP LogLine
    parser = LogLine <$  char '[' <*> pDate <* char ']' <* skipSpaces <*> pAction

pNumber :: ReadP Integer
pNumber = read <$> munch1 isNumber

pDate :: ReadP LocalTime
pDate = do
  s <- munch1 (/= ']') -- This is bad but hey, not real code :)
  parseTimeM True defaultTimeLocale "%F %H:%M" s

pAction :: ReadP Action
pAction = (BeginShift <$ string "Guard #" <*> pNumber <* string " begins shift")
      +++ (FallAsleep <$ string "falls asleep")
      +++ (WakeUp     <$ string "wakes up")

runParser :: ReadP a -> String -> Maybe a
runParser parser input = 
  case [ x | (x,"") <- readP_to_S parser input ] of
    [x] -> Just x
    []  -> Nothing
    _   -> Nothing

-- | diffLocalTime a b = a - b. Only exists in time 1.9+
diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)
