module Chapter6.KMeans where

class Vector v where
  distance :: v -> v -> Double
