module Chapter5.Problems where

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
  where sumForce' [] z = z
        sumForce' (y:ys) z = let s = z + y
                             in  s `seq` sumForce' ys s
