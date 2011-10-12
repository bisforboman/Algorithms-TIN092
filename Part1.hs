module Main where

import Data.List
import Control.Monad (replicateM)

type Path = [Int] -- e.g. [x,y,z] ==> v1 = x, v2 = y, v3 = z
type Graph = ([[Int]], [Double]) -- (cost_matrix, probabilities)


selections :: [a] -> [(a, [a])]
selections [] = 
selections (x:xs) = 

myPermutations :: [a] -> [[a]]
myPermutations [] = [[]]
myPermutations (x:xs) = [ | ys <- myPermutations xs]

-- | Given number of nodes, generate all possible paths
generatePaths :: Int -> [Path]
generatePaths n = map (0:) $ myPermutations [1..n-1]

pathCost :: Graph -> Path -> Double
pathCost (edgeMatrix, costs) path = 
    sum $ zipWith aux accumulativeEdgeCosts path
  where
    edges :: [(Int, Int)]
    edges = path `zip` tail path
    edgeCosts :: [Int]
    edgeCosts = map (\(from,to) -> edgeMatrix !! from !! to) edges
    accumulativeEdgeCosts :: [Int]
    accumulativeEdgeCosts = scanl (+) 0 edgeCosts
    aux :: Int -> Int -> Double
    aux acc ix = fromIntegral acc * (costs !! ix)
    

-- The master function
bestPathAndCost :: Graph -> (Double, Path)
bestPathAndCost g@(edgeMatrix, costs) = minimum latenciesAndPaths
  where
    latenciesAndPaths = map (pathCost g) paths `zip` paths
    paths = generatePaths (length edgeMatrix)
    
    
main :: IO ()
main = do
    n <- readLn :: IO Int
    costs <- replicateM n (readLn :: IO Double)
    edges <- replicateM n $ fmap (map (read :: String -> Int) . words) getLine
    let (res, path) = bestPathAndCost (edges, costs)
    putStrLn $ "Min. Expected Latency: " ++ show res
    putStrLn $ "Path: " ++ unwords (map (show . (+1)) path)
