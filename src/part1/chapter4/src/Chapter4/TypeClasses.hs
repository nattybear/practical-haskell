module Chapter4.TypeClasses where

class Nameable n where
  name :: n -> String
