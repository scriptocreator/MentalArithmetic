{-# LANGUAGE DuplicateRecordFields, StrictData #-}

module TypeAbacus where



type Expression = [[Abacus]]

data Abacus
    = Abacus [RowAbacus]
    | Plus
    | Minus
    deriving Show

data RowAbacus = RowAbacus [Done] Bool deriving Show

data Done = Done deriving (Show, Eq, Ord)


data App = App
    {operator :: Bool
    , function :: RowAbacus -> (Int, Int)}

data AppBro = AppBro
    {operator :: Bool
    , ifPlus :: Bool
    , function :: Bool -> RowAbacus -> (Int, Int)}


type Carriage = Int


data Theme = Merely | Brother | Friend deriving (Show, Eq, Ord)

type StartLine = Int
type LengthExpr = Int
type RangeRows = (Int, Int)
