module Main where

import Control.Concurrent
import GenAlg
import Tour
import City
import Config
import System.Random
import Graphics.EasyPlot

plotTour :: Tour -> IO Bool
plotTour t@(Tour cs) = do
  threadDelay 200000
  plot (PNG "plot.png") $
    Data2D [Title title, Style Linespoints] [] lst
  where lst' = map toPair cs
        lst = lst' ++ [head lst']
        title = show (len t)

main :: IO ()
main = do
  gen <- newStdGen
  initPop <- return $ initialPopulation gen
  mapM_ plotTour $ nBests gen 500 initPop
