module Main where

import Data.List
import Control.Monad (replicateM, forM_)

type Path = [Int] -- e.g. [x,y,z] ==> v1 = x, v2 = y, v3 = z
type Graph = ([[Int]], [Double]) -- (cost_matrix, probabilities)
type Strategy = Graph -> Path -> Int

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
bruteForce :: Graph -> (Double, Path)
bruteForce g@(edgeMatrix, costs) = minimum latenciesAndPaths
  where
    latenciesAndPaths = map (pathCost g) paths `zip` paths
    paths = map (0:) $ permutations [1..(length edgeMatrix)-1]

useGreedy :: Strategy -> Graph -> (Double, Path)
useGreedy strat graph@(_,costs) = (pathCost graph path, path)
  where path = reverse rpath
        rpath = iterate f [0] !! (n-1)
        f :: Path -> Path
        f p = strat graph p : p
        n = length costs


stratCheapEdge :: Strategy
stratCheapEdge (edgeMatrix, _) visited@(current:_) = snd $ minimum $ indexAndPay
  where n = length edgeMatrix
        edges = edgeMatrix !! current
        indexAndPay = [(pay, i) | i <- [0..n-1], i `notElem` visited, let pay = edges !! i]

stratCheapProb :: Strategy
stratCheapProb (_, costs) visited@(current:_) = snd $ maximum $ indexAndPay
  where n = length costs
        indexAndPay = [(pay, i) | i <- [0..n-1], i `notElem` visited, let pay = costs !! i]

main :: IO ()
main = do
    n <- readLn :: IO Int
    costs <- replicateM n (readLn :: IO Double)
    edges <- replicateM n $ fmap (map (read :: String -> Int) . words) getLine
    let algs = [("greedy1", useGreedy stratCheapEdge)
              , ("greedy2", useGreedy stratCheapProb)
              , ("bruteforce", bruteForce)]
    forM_ algs $ \(name, alg) -> do
        let (res, path) = alg (edges, costs)
        putStrLn $ "Algorithm: " ++ name
        putStrLn $ "\tMin. Expected Latency: " ++ show res
        putStrLn $ "\tPath: " ++ unwords (map (show . (+1)) path)
