{-# LANGUAGE RecordWildCards #-}

module Chapter4.TypeClasses where

import qualified Chapter2.DataTypes as T
import           Chapter3.ParamPoly

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n } }
    = f ++ " " ++ n
  name c = clientName c

class Priceable p where
  price :: p -> Double

instance Priceable T.TimeMachine where
  price = T.price

totalPrice :: Priceable p => [p] -> Double
totalPrice = sum . map price

instance Eq i => Ord (Client i) where
  compare c1 c2 = case compare (name c1) (name c2) of
                    LT -> LT
                    GT -> GT
                    EQ -> case (c1, c2) of
                            (Individual { .. }, _) -> GT
                            (_, Individual { .. }) -> LT
                            _                      -> compare (duty c1)
                                                              (duty c2)
