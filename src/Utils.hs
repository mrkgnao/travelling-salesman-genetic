module Utils where

import qualified Data.Map as M
import System.Random

randomList :: Random a => a -> a -> Int -> StdGen -> ([a], StdGen)
randomList p q len gen =
  randSeq gen (\g _ -> randomR (p,q) g) (replicate len 0)

randomName gen =
  let (str,gen') = randomList 'a' 'z' 6 gen
  in  (tail str, gen')

swap i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x

randomAccum :: StdGen
            -> a
            -> Int
            -> (StdGen -> a -> (a, StdGen))
            -> ([a], StdGen)
randomAccum gen a 0 _ = ([a], gen)
randomAccum gen a n f =
  let (a',gen') = f gen a
      (as,gen'') = randomAccum gen' a' (n-1) f
  in  (a':as,gen'')

(!>) = drop
(<!) = flip take

infix 1 !>
infix 2 <!

fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen = (M.singleton 0 x, gen)

randSeq :: StdGen -> (StdGen -> a -> (b, StdGen)) -> [a] -> ([b], StdGen)
randSeq gen f [] = ([], gen)
randSeq gen f [a] =
  let (b,gen') = f gen a
  in ([b],gen')
randSeq gen f (a:as) =
  let (b,gen') = f gen a
      (bs,gen'') = randSeq gen' f as
  in (b:bs,gen'')

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"
