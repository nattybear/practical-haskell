{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter6.KMeans where

import           Data.List
import qualified Data.Map as M

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst
                     n = fromIntegral $ length lst
                 in  (u / n, v / n)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> [e] -> [v]
kMeans = undefined

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v)
                       => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
  in  foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                     in  M.adjust (p:) chosenC m)
            initialMap points
  where compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)
