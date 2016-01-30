module City where

import System.Random
import Utils

data City = City Double Double String
  deriving Eq

randomlyNamedCity :: (Double, Double) -> City
randomlyNamedCity (x,y) = City x y name
  where (name,_) = randomName (mkStdGen seed)
        seed = round (x + y) :: Int

instance Show City where
  show (City x y str) =
    case str of
      "" -> show (x,y)
      s  -> str

toPair :: City -> (Double, Double)
toPair (City x y _) = (x,y)

sqDist :: City -> City -> Double
sqDist (City x1 y1 _) (City x2 y2 _) = (x1 - x2) ** 2 + (y1 - y2) ** 2

dist :: City -> City -> Double
dist c1 c2 = sqrt (sqDist c1 c2)
