{-# LANGUAGE DuplicateRecordFields, StrictData #-}

module TypeAbacus where


data Done = Done deriving (Show, Eq, Ord)

data RowAbacus = RowAbacus [Done] Bool deriving Show

data App = App
    {operator :: Bool
    , function :: (RowAbacus -> (Int, Int))}

data AppBro = AppBro
    {operator :: Bool
    , ifPlus :: Bool
    , function :: (Bool -> RowAbacus -> (Int, Int))}

type Abacus = [RowAbacus]
