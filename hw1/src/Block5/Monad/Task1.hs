module Block5.Monad.Task1
  ( Expr(..)
  , ArithmeticError(..)
  ) where

data Expr
  = Var Int
  | Sum Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

data ArithmeticError
  = DivisionByZero
  | NegatePow
