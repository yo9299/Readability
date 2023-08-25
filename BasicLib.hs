module BasicLib  where

import qualified Data.Map as M
import Control.Monad.State
import Data.List
import Debug.Trace


data Vertex = Source Int | Target Int 
  deriving (Ord, Eq, Show)
type Adjacencies = M.Map Vertex [Vertex]
type LabelList = M.Map Vertex String
type EdgeList = M.Map (Vertex, Vertex) Int
data Graph = Graph {_vertices:: LabelList , _edges:: EdgeList, _adjacencies
                     :: Adjacencies, _availablel:: String}
  deriving (Show, Eq)


constructGraph::[(Vertex, String)] -> [((Vertex, Vertex), Int)] -> Graph
constructGraph labels edges = Graph {_vertices = (M.fromList labels), _edges = (M.fromList edges), _adjacencies=(constructAdjacencies (edges)), _availablel = ['a'..'z']}
  
putLabel :: Graph  -> Vertex -> String -> Graph
putLabel g v mylabel = g {_vertices = M.insert v mylabel (_vertices g)}

getLabel :: Graph -> Vertex -> String
getLabel g v =case M.lookup v (_vertices g) of
                    Nothing -> error "vertex" ++ (show v) ++ "does not exist"
                    Just label -> label

getPref :: Graph ->  Vertex -> Int -> String
getPref g v k = take k mylabel
  where mylabel = getLabel g  v 

getSuf :: Graph -> Vertex -> Int -> String
getSuf g v k = drop ((length mylabel ) -k) mylabel
   where mylabel = getLabel g v
   

getSources :: Graph -> [Vertex]
getSources g =  [Source k |k <-[0..(M.size (_adjacencies g))],  M.lookup (Source k) (_adjacencies g) /= Nothing]

getTargets :: Graph -> [Vertex]
getTargets g = [Target k | k <-[0..(M.size (_adjacencies g))], M.lookup (Target k) (_adjacencies g) /= Nothing]

--get list of neighbors of a vertex                        
getNeighborhood :: Graph ->  Vertex -> [Vertex]
getNeighborhood g v = case M.lookup v (_adjacencies g) of
                               Nothing -> error "vertex does not exist"
                               Just nbh -> nbh
                               
--get list of non neighbors of a vertex
getNonNbhd :: Graph -> Vertex -> [Vertex]
getNonNbhd g v@(Source v0) =  (getTargets g) \\ ( getNeighborhood g v)
getNonNbhd g  v@(Target v0) = listv \\ ( getNeighborhood g v)
  where listv =getSources g
      

-- given EdgeList and edge, returns its weight
getEdgeWeight :: Graph -> (Vertex, Vertex) -> Int
getEdgeWeight g edge@(Source u, Target v) =case  M.lookup edge (_edges g) of
                                            Nothing -> error "edge does not exist"
                                            Just e -> e

getEdgeWeight g edge@(Target u, Source v) =case  M.lookup (Source v, Target u) (_edges g) of
                                            Nothing -> error "edge does not exist"
                                            Just e -> e


getListEdges:: Graph -> [(Vertex,Vertex)]
getListEdges g = [(Source u,Target v) |u<-[0..(M.size (_edges g))], v<-[0..(M.size (_edges g))], M.lookup (Source u,Target v) (_edges g) /= Nothing]
  
getAdjacencies :: Adjacencies -> [(Vertex,Vertex)]-> Adjacencies
getAdjacencies initial [] = initial
getAdjacencies initial edges = getAdjacencies (M.insertWith (++) x [y] adj0) rest 
  where adj0= M.insertWith (++) y [x] initial
        (x,y) = head edges
        rest = tail edges

--computes the adjacencies given the input for the graph
constructAdjacencies :: [((Vertex, Vertex), Int)] -> Adjacencies
constructAdjacencies liste = getAdjacencies initial (fmap fst liste)
  where initial = M.empty



-- checks whether there exists an overlap between a suffix of String 1 and a prefix of string 2. Beware to call with Source and Target
existOverlap :: Graph -> Vertex -> Vertex -> Bool
existOverlap l v@(Source v0) v'@(Target y) = or [ getSuf l v k == getPref l v' k | k<- [1..(n)]]
  where n = length $ getLabel l v
existOverlap l v'@(Target y)  v@(Source v0) = or [ getSuf l v k == getPref l v' k | k<- [1..(n)]]
   where n = length $ getLabel l v

--checks if two vertices are compatible with weight k, 1st is source, 2 target -- but doesn't work very well
compatibleOv :: Graph -> Vertex -> Vertex -> Int -> Bool
compatibleOv g  v'@(Target y) v@(Source x) k = compatibleOv g  v v' k
compatibleOv g v@(Source x) v'@(Target y) k
  | or [null $ getPref g v' k, null $ getSuf g v k] = True
  | and [ks == k, kt == k, getSuf g v k == getPref g v' k] = True
  | ks + kt <= k = True
  | (kt < k && ks < k) = (getSuf g v' (kt - k + ks)  == getPref g v (ks -k +kt))
  | ks < k = (getSuf g v ks == drop (k-ks) (getPref g v' k))
  | kt < k = (getPref g v' kt == take kt (getSuf g v k))
 
  | otherwise = False
      where ks = (length $ getSuf g v k)
            kt = (length $ getPref g v' k)

       
-- returns True if an undesired edge appears (overlap between non-edges), False if no new overlaps are created 
existUndesiredOverlaps :: Graph  -> Bool
existUndesiredOverlaps g  = (or $ fmap (existUndesiredOverlap g) lvert)  --(( not.and) $ fmap (exactOverlap g) lvert)
  where lvert = getSources g  ++ getTargets g
        existUndesiredOverlap g0 v = or $ fmap (existOverlap g0  v) (getNonNbhd g0 v)


-- returns True if the overlaps are all according to weights 
exactOverlap :: Graph -> Bool
exactOverlap g  =((and) $ fmap (exactv g) lvert)
          where exactv g v = and $ fmap ( exactOverlapEdge' g v) nbs
                  where nbs = getNeighborhood g v
                lvert = getSources g  ++ getTargets g

-- given 2 vertices, returns True if the overlap of the edge is according to the wight
exactOverlapEdge' :: Graph -> Vertex -> Vertex -> Bool
exactOverlapEdge' g y@(Target y0) x@(Source x0)  = exactOverlapEdge' g x y 
exactOverlapEdge' g x@(Source x0) y@(Target y0)  = compatibleOv g x y k && ((length $ getSuf g x k) == (length $ getSuf g y k)) && ((length $ getSuf g x k) ==k)
  where k = getEdgeWeight  g (x, y)
  
                                            
isUnlabeled :: Graph -> Vertex -> Bool
isUnlabeled g v = if M.lookup v (_vertices g) == Just ( "")
                      then True
                           else False

                                
isVertexSatisfied :: Graph -> Vertex -> Bool
isVertexSatisfied g v = and $ fmap (exactOverlapEdge'  g v) nbs
  where nbs = getNeighborhood  g v
        weights = fmap ( getEdgeWeight  g) (zip (repeat v) nbs)

getLabeledVert :: Graph -> [Vertex]
getLabeledVert g = filter (\x -> not $ isUnlabeled g x) vert
  where vert = getSources g ++ getTargets g

getUnlabeledVert :: Graph -> [Vertex]
getUnlabeledVert g = filter (\x -> isUnlabeled g x) vert
  where vert = getSources g ++ getTargets g

getSatisfiedVert :: Graph -> [Vertex]
getSatisfiedVert g = filter (\x -> isVertexSatisfied g x) vert
    where vert = getSources g ++ getTargets g

getUnsatisfiedVert :: Graph -> [Vertex]
getUnsatisfiedVert g = filter (\x -> not $ isVertexSatisfied g x) vert
    where vert = getSources g ++ getTargets g


getUnsatisfiedNbs :: Graph -> Vertex -> [Vertex]
getUnsatisfiedNbs g v = filter (\x -> (not $ exactOverlapEdge' g v x)) nbs
  where nbs = getNeighborhood g v

getNbsWeight :: Graph -> Vertex -> Int -> [Vertex]
getNbsWeight g v k= filter (\x -> ((getEdgeWeight g (v, x)) == k)) nbs
  where nbs = getNeighborhood g v

getLabeledVert2 :: Graph -> [Vertex]
getLabeledVert2 g = filter (\x -> (x `elem` getNbsWeight g x 2)) (getLabeledVert g)

-- unsatnbs' = filter (\x -> getEdgeWeight g (v,x) == ((length $ getLabel g v) + k)) unsatnbs

