module GenAlg where

import Config
import Utils
import Tour
import System.Random
import Data.List (sortOn)

mutate :: StdGen -> Tour -> (Tour, StdGen)
mutate gen (Tour tour) =
  let l            = length tour
      (ix1, gen')  = randomR (0,l-1) gen
      (ix2, gen'') = randomR (0,l-1) gen'
  in (Tour $ swap ix1 ix2 tour, gen'')

crossover :: StdGen -> (Tour, Tour) -> (Tour, StdGen)
crossover gen ((Tour s),(Tour t)) =
  let l               = length t
      (ix1, gen')     = randomR (0,l-1) gen
      (ix2, gen'')    = randomR (0,l-1) gen'
      (a,b)           = (min ix1 ix2, max ix1 ix2)
      sub             = a !> s <! b
      (before, after) = splitAt a t
      [before', after']
        = map (filter (`notElem` sub)) [before, after]
  in (Tour $ before' ++ sub ++ after', gen'')

nextGen :: StdGen -> Population -> (Population, StdGen)
nextGen gen pop =
  let
    -- Mutate the whole population
    (mutated, gen1) = randSeq gen mutate pop
    -- Add mutated clones to population
    pop1 = pop ++ mutated
    -- Create a randomly permuted copy of the population
    (randomizedPop, gen2) = fisherYates gen1 pop1
    -- Zip into pairs
    parentPairs = zip pop1 randomizedPop
    -- Ahem
    (children, gen3) = randSeq gen2 crossover parentPairs
    -- Add children to population
    pop2 = pop1 ++ children
    -- Select fittest members of population
    finalPop = take popSize $ sortOn len pop2
  in (finalPop, gen3)

nGens :: StdGen -> Int -> Population -> [Population]
nGens gen n pop =
  fst $ randomAccum gen pop n nextGen

nBests :: StdGen -> Int -> Population -> [Tour]
nBests gen n pop = map (head . sortOn len) $ nGens gen n pop
