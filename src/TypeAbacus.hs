{-# LANGUAGE FlexibleInstances, DuplicateRecordFields, StrictData #-}

module TypeAbacus where



data Abacus
    = Abacus [RowAbacus]
    | Plus
    | Minus
    deriving (Show, Eq)

data RowAbacus = RowAbacus [Done] Bool deriving Show

data Done = Done deriving (Show, Eq, Ord)

instance Eq RowAbacus where
    RowAbacus leftLower leftUpper == RowAbacus rightLower rightUpper
        | (leftLower == rightLower) && (leftUpper == rightUpper) = True
        | otherwise = False

    leftRow /= rightRow = not $ leftRow == rightRow


data Expr
    = Expr {operator :: Bool}
    | ExprBro {operator :: Bool, ifPlus :: Bool} -- Оператор; Для условия функции
