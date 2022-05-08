{-# LANGUAGE ViewPatterns #-}
module Chapter3.Ranges (Range(), range) where

data Range = Range Integer Integer deriving Show

range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b

prettyRange :: Range -> String
prettyRange rng = case rng of
                    (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"
