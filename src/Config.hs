module Config where

import City
import Tour
import Utils
import System.Random

popSize :: Int
popSize = 100

tourLength :: Int
tourLength = 30

basicTour :: Tour
basicTour =
  Tour $ map randomlyNamedCity coordPairs
  where
    gen = mkStdGen 0
    (randX, gen') = randomList 0 100 (tourLength * 2) gen
    (randY, _) = randomList 0 100 (tourLength * 2) gen'
    coordPairs = zip randX randY

initialPopulation :: StdGen -> Population
initialPopulation gen = map Tour . fst $ randomAccum gen cs 50 fisherYates
  where (Tour cs) = basicTour
