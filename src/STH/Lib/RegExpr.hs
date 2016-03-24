module STH.Lib.RegExpr where

data CharClass
  = Alpha
  | AlNum
  | Punct
  | Digit
  deriving (Eq, Show)

data RegEx
  = Literal  Char
  | Range    Char Char
  | AnyChar
  | OneOf    CharClass
  | Choice   [RegEx]
  | Sequence [RegEx]
  | Kleene   RegEx
  deriving (Eq, Show)
