module Part1 where

type Path = [Int] -- e.g. [x,y,z] ==> v1 = x, v2 = y, v3 = z
type Graph = (Int, [[Int]], [Double]) -- (num_nodes, cost_matrix, probabilities)

