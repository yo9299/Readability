module Propagate where

import BasicLib
import qualified Data.Map as M
import Control.Monad.State
import Data.List
import Debug.Trace


   


propagate :: Graph -> Vertex -> Vertex  -> Graph
propagate g (Target y) (Source x) = propagate g  (Source x) (Target y)
propagate g  vs@(Source x) vt@(Target y) 
  | not $ compatibleOv g vs vt k = error "labels are not compatible"
  | (null ls && null lt) =  (putLabel (putLabel g vt (take k (_availablel g))) vs (take k (_availablel g))) {_availablel = drop k (_availablel g)}
  | (null ls && kt == k) = (putLabel g  vs extendedt)
  | (null ls && kt < k) = (putLabel (putLabel g vs extendedt)  vt extendedt)  {_availablel = drop (k-kt) (_availablel g)}
  | (null lt && ks == k) = (putLabel g vt extendeds)
  | (null lt && ks < k) = (putLabel (putLabel g vs extendeds)  vt extendeds)  {_availablel = drop (k-ks) (_availablel g)}
  | (ks + kt <= k) = (putLabel (putLabel g vs merge) vt merge) {_availablel = drop (k-ks-kt) (_availablel g)}
  | ks < k && kt < k = (putLabel (putLabel g vs merge1) vt merge1)
  | ks < k = putLabel g vs lt
  | kt < k = putLabel g vt ls
  | otherwise = g
          where ls = getSuf g vs k --getLabel g vs 
                lt = getPref g vt k --getLabel g vt 
                ks = length ls
                kt = length lt
                merge = lt ++ (take (k-ks-kt) (_availablel g)) ++ ls
                merge1 = (take (k-ks) lt) ++ ls
                extendedt = lt ++ take (k-kt) (_availablel g)
                extendeds = take (k-ks) (_availablel g) ++ ls
                k = getEdgeWeight g (vs, vt)

propagateVertexSafe :: Graph -> Vertex -> Graph
propagateVertexSafe g v = foldl (\acc x -> propagate acc v x) g safenbs
  where nbs = getNeighborhood g v
        safenbs = filter (\x -> getEdgeWeight g (v,x) <= ks) nbs
        ks = length $ getLabel g v


propagateSafe :: Graph -> Graph
propagateSafe g = foldl (\acc x -> propagateVertexSafe acc x) g vert
  where vert = getSources g ++ getTargets g 

-- should take a labeled vertex and propagate it to edge of only length +1
--if unsatnbs' empty, then take +2, or take another v
propagateNew :: Graph -> Graph
propagateNew g = propagate g v v'
  where v =trace (show $ getQueue g) fst $ getQueue g
        v' = snd $ getQueue g 

  
getQueue :: Graph -> (Vertex, Vertex)
getQueue g
  | null $ getLabeledVert g = (head vert, head $ getNeighborhood g (head vert))
  | otherwise = (head queue, head nb)
                          where vert = getSources g ++ getTargets g
                                queue =if null $ intersect (getUnsatisfiedVert g) (getLabeledVert2 g) then intersect (getUnsatisfiedVert g) (getLabeledVert g)
                                  else intersect (getUnsatisfiedVert g) (getLabeledVert2 g)
                                nb = if null $ intersect (getUnsatisfiedNbs g (head queue)) (getNbsWeight g (head queue) 2)
                                  then getUnsatisfiedNbs g (head queue)
                                  else intersect (getUnsatisfiedNbs g (head queue)) (getNbsWeight g (head queue) 2)
                                  

propagateCombined :: State Graph ()
propagateCombined = do
  cur <- get
  let newg = propagateSafe cur
  if cur == newg
    then do
        let g' = propagateNew cur
        put g'
             else do
             put newg 

goalCondition :: Graph -> Bool
goalCondition g = exactOverlap g && (not $ existUndesiredOverlaps g)

stopCondition :: Graph -> Bool
stopCondition g = existUndesiredOverlaps g

algo :: State Graph (Maybe Graph)
algo = do
  cur <- get
  if goalCondition cur
    then pure $ (Just cur)
  else if stopCondition cur
    then pure $ Nothing
  else do
    propagateCombined
    algo 
