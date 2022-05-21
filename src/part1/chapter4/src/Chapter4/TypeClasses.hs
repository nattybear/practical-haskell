module Chapter4.TypeClasses where

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)
