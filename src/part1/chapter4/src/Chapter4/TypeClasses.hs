module Chapter4.TypeClasses where

import qualified Chapter2.DataTypes as T (TimeMachine (..))
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
