module Chapter4.TypeClasses where

import Chapter2.DataTypes (TimeMachine)
import Chapter3.ParamPoly

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n } }
    = f ++ " " ++ n
  name c = clientName c

class Priceable p where
  totalPrice :: [p] -> Double
