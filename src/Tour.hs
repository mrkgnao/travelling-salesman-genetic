module Tour where

import City

data Tour = Tour [City]
type Population = [Tour]

instance Show Tour where
  show t = show (len t)

shiftToEnd :: [a] -> [a]
shiftToEnd [] = []
shiftToEnd [x] = [x]
shiftToEnd (x:xs) = xs ++ [x]

len :: Tour -> Double
len (Tour cs) = sum . map (uncurry sqDist) $ zip cs (shiftToEnd cs)
