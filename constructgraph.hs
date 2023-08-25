module Constructgraph where

import BasicLib
import Propagate
import Data.List
import Debug.Trace
import Control.Monad.State
import qualified Data.Map as M

buildVertices :: Int -> Int -> [(Vertex, String)]
buildVertices s t = zip myVert (repeat "")
  where myVert= fmap Source (take s [0,2..])  ++ fmap Target (take t [1,3..])

buildEdges :: [(Int, Int)] -> [Int] -> [((Vertex, Vertex), Int)]
buildEdges edges weights = zip (fmap f edges) weights
  where f (s,t )
          | (s `mod` 2 == 0) && (t `mod`2 == 1) = (Source s, Target t)
          | (s `mod`2 == 1) && (t `mod`2 == 0) = (Source t, Target s)
          | otherwise =  error  "illegal edge"

buildGraph :: Int -> Int -> [(Int, Int)] -> [Int] -> Graph
buildGraph s t edges weights = constructGraph (buildVertices s t) (buildEdges edges weights) 
          
gadget = buildGraph 15 15 (take 42 [(0,1), (1,2), (2,3), (3, 4), (4,5), (5,0), (0,7) , (1,6), (6,7), (6,9), (7,8), (8,9), (1, 10), (2,11), (10, 11), (10,13), (11,12), (12,13), (2,15), (3,14), (14,15), (15,16), (14, 17), (16, 17), (3,18), (4, 19), (18,19), (18, 21), (19,20), (20,21), (4, 23), (5,22), (22,23), (22,25), (23,24), (24,25), (5,26), (0,27), (26,27), (26, 29), (27,28), (28,29)]) (take 40 [1,2,1,2,1,2, 1,1,1,3,3,2, 3,3,1,1,1,1, 1,1,1,3,3,2, 3,3,1,1,1,1, 1,1,1,3,3,2, 3,3,1,1,1,1])

--gadget = buildGraph 3 3  [(0,1), (1,2), (2,3), (3, 4), (4,5), (5,0), (0,7)] [1,2,1,2,1,2]

-- , (1, 10), (2,11), (10, 11), (10,13), (11,12), (12,13), (2,15), (3,14), (14,15), (15,16), (14, 17), (16, 17), (3,18), (4, 19), (18,19), (18, 21), (19,20), (20,21), (4, 23), (5,22), (22,23), (22,25), (23,24), (24,25), (5,26), (0,27), (26,27), (26, 29), (21,28), (28,29)]



test = Graph {_vertices = M.fromList [(Source 0,"cba"),(Source 2,"fad"),(Source 4,"edb"),(Source 6,"gha"),(Source 8,"agh"),(Source 10,"adf"),(Source 12,"f"),(Source 14,"ijd"),(Source 16,"dij"),(Source 18,"dbe"),(Source 20,"e"),(Source 22,"klb"),(Source 24,"bkl"),(Source 26,"bac"),(Source 28,"c"),(Target 1,"adf"),(Target 3,"dbe"),(Target 5,"bac"),(Target 7,"agh"),(Target 9,"gha"),(Target 11,"fad"),(Target 13,"f"),(Target 15,"dij"),(Target 17,"ijd"),(Target 19,"edb"),(Target 21,"e"),(Target 23,"bkl"),(Target 25,"klb"),(Target 27,"cba"),(Target 29,"c")], _edges = M.fromList [((Source 0,Target 1),1),((Source 0,Target 5),2),((Source 0,Target 7),1),((Source 0,Target 27),3),((Source 2,Target 1),2),((Source 2,Target 3),1),((Source 2,Target 11),3),((Source 2,Target 15),1),((Source 4,Target 3),2),((Source 4,Target 5),1),((Source 4,Target 19),3),((Source 4,Target 23),1),((Source 6,Target 1),1),((Source 6,Target 7),1),((Source 6,Target 9),3),((Source 8,Target 7),3),((Source 8,Target 9),2),((Source 10,Target 1),3),((Source 10,Target 11),1),((Source 10,Target 13),1),((Source 12,Target 11),1),((Source 12,Target 13),1),((Source 14,Target 3),1),((Source 14,Target 15),1),((Source 14,Target 17),3),((Source 16,Target 15),3),((Source 16,Target 17),2),((Source 18,Target 3),3),((Source 18,Target 19),1),((Source 18,Target 21),1),((Source 20,Target 19),1),((Source 20,Target 21),1),((Source 22,Target 5),1),((Source 22,Target 23),1),((Source 22,Target 25),3),((Source 24,Target 23),3),((Source 24,Target 25),2),((Source 26,Target 5),3),((Source 26,Target 27),1)], _adjacencies =M.fromList [(Source 0,[Target 27,Target 7,Target 5,Target 1]),(Source 2,[Target 15,Target 11,Target 3,Target 1]),(Source 4,[Target 23,Target 19,Target 5,Target 3]),(Source 6,[Target 9,Target 7,Target 1]),(Source 8,[Target 9,Target 7]),(Source 10,[Target 13,Target 11,Target 1]),(Source 12,[Target 13,Target 11]),(Source 14,[Target 17,Target 15,Target 3]),(Source 16,[Target 17,Target 15]),(Source 18,[Target 21,Target 19,Target 3]),(Source 20,[Target 21,Target 19]),(Source 22,[Target 25,Target 23,Target 5]),(Source 24,[Target 25,Target 23]),(Source 26,[Target 27,Target 5]),(Target 1,[Source 10,Source 6,Source 2,Source 0]),(Target 3,[Source 18,Source 14,Source 4,Source 2]),(Target 5,[Source 26,Source 22,Source 4,Source 0]),(Target 7,[Source 8,Source 6,Source 0]),(Target 9,[Source 8,Source 6]),(Target 11,[Source 12,Source 10,Source 2]),(Target 13,[Source 12,Source 10]),(Target 15,[Source 16,Source 14,Source 2]),(Target 17,[Source 16,Source 14]),(Target 19,[Source 20,Source 18,Source 4]),(Target 21,[Source 20,Source 18]),(Target 23,[Source 24,Source 22,Source 4]),(Target 25,[Source 24,Source 22]),(Target 27,[Source 26,Source 0])], _availablel = "mnopqrstuvwxyz"}
