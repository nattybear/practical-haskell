module Chapter3.Compr where

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
withPositions list = zip [1 .. length list] list

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds list = [ 2 * x | x <- list, odd x ]
