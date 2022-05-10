{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards   #-}

module Chapter3.Compr where

import Chapter3.Lists
import Chapter3.ParamPoly
import GHC.Exts

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
withPositions list = zip [1 .. length list] list

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds list = [ 2 * x | x <- list, odd x ]

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
                           | client@(Company { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client
                           ]
