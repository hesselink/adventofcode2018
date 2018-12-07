module Main where

import Data.Char
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

main :: IO ()
main = do
  f <- readFile "input"
  let depList = mapMaybe parseDependency . lines $ f
      allSteps = Set.fromList . concatMap (\dep -> [item dep, dependsOn dep]) $ depList
      deps = buildDependencies depList
      res = resolveSteps allSteps deps
  putStrLn $ "Answer one: " ++ show res
  let res2 = resolveParallel allSteps deps
  putStrLn $ "Answer two: " ++ show res2

resolveSteps :: Set Step -> Dependencies -> [Step]
resolveSteps allSteps deps =
  case listToMaybe $ withoutDependencies allSteps $ deps of
    Nothing -> []
    Just nextStep ->
      let newDeps = resolveDependency nextStep deps
          newSteps = Set.delete nextStep allSteps
      in nextStep : resolveSteps newSteps newDeps

resolveParallel :: Set Step -> Dependencies -> Int
resolveParallel allSteps deps = go allSteps deps []
  where
    go :: Set Step -> Dependencies -> [Work] -> Int
    go stepsLeft depsLeft workInProgress =
      case (workInProgress, withoutDependencies stepsLeft $ depsLeft) of
        ([], []) -> -1 -- There's an off by one error somewhere...
        (_ , _) ->
          let newWork = timeStep workInProgress
              stepsFinished = map fst workInProgress \\ map fst newWork
              newDeps = foldr resolveDependency depsLeft stepsFinished
              nextSteps = withoutDependencies stepsLeft newDeps
              stepsToStart = take (5 - length newWork) nextSteps
              newStepsLeft = stepsLeft `Set.difference` (Set.fromList stepsToStart)
              newWorkInProgress = map startStep stepsToStart ++ newWork
          in traceShow (workInProgress, newWork) $ 1 + go newStepsLeft newDeps newWorkInProgress

type Work = (Step, Int)

timeStep :: [Work] -> [Work]
timeStep = mapMaybe timeStep1
  where
    timeStep1 (_, 1) = Nothing
    timeStep1 (i, n) = Just (i, n - 1)

startStep :: Step -> Work
startStep s = (s, ord s - 4) -- A is 65 in ascii, 1 in assignment, add 60 seconds

type Step = Char

data Dependency = Dependency
  { item :: Step
  , dependsOn :: Step
  } deriving Show

parseDependency :: String -> Maybe Dependency
parseDependency s = do
  (dep:rest) <- stripPrefix "Step " s
  (i:_) <- stripPrefix " must be finished before step " rest
  return $ Dependency { item = i, dependsOn = dep }

buildDependencies :: [Dependency] -> Dependencies
buildDependencies = foldr (\dep -> addDependency (item dep) (dependsOn dep)) emptyDependencies

-- returned list is ordered
withoutDependencies :: Set Step -> Dependencies -> [Step]
withoutDependencies allSteps deps
  = map fst
  . filter (Set.null . snd)
  . map (\i -> (i, getDependencies i deps))
  . Set.toAscList
  $ allSteps

data Dependencies = Dependencies
  { mapping :: Map Step (Set Step)
  , reverseMapping :: Map Step (Set Step)
  } deriving Show

emptyDependencies :: Dependencies
emptyDependencies = Dependencies { mapping = Map.empty, reverseMapping = Map.empty }

getDependencies :: Step -> Dependencies -> Set Step
getDependencies i = fromMaybe Set.empty . Map.lookup i . mapping

addDependency :: Step -> Step -> Dependencies -> Dependencies
addDependency i dep deps = Dependencies
  { mapping        = Map.insertWith Set.union i   (Set.singleton dep) . mapping $ deps
  , reverseMapping = Map.insertWith Set.union dep (Set.singleton i)   . reverseMapping $ deps
  }

resolveDependency :: Step -> Dependencies -> Dependencies
resolveDependency dep deps =
  let mItems = Map.lookup dep . reverseMapping $ deps
      removeDependency d i ds = Map.adjust (Set.delete d) i ds
  in case mItems of
       Nothing -> deps
       Just items -> Dependencies
         { mapping = foldr (removeDependency dep) (mapping deps) items
         , reverseMapping = Map.delete dep . reverseMapping $ deps
         }
