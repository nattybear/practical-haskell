{-# LANGUAGE FlexibleInstances #-}

module Chapter6.KMeans where

class Vector v where
  distance :: v -> v -> Double

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
