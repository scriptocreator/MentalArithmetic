{-# LANGUAGE DuplicateRecordFields, Strict, StrictData #-}

module TypeImperativeAbacus where

import TypeAbacus

import System.Random
import System.Random.Stateful

infixr 6 :\^/


data World
    = EditApp [EditApp]
    | EditSettings [EditSet]

    | App Expressions StdGen Carriage
    | Settings StartLine LengthExpr QuantityQuestion Theme RangeRows

    | World :\^/ World

data EditApp
    = EditAppVoid
    | EditExpressions Expressions
    | EditRandom StdGen
    | EditCarriage Carriage
    deriving (Show, Eq)

data EditSet
    = EditSetVoid
    | EditStartLine {editStart :: Either TypeTag StartLine}
    | EditLengthExpr {editExpr :: Either TypeTag LengthExpr}
    | EditQuantityQuestion {editQuest :: Either TypeTag QuantityQuestion}
    | EditTheme {editTheme :: Either TypeTag Theme}
    | EditRangeRows {nested :: Bool, direct :: Direct, editRange :: Either TypeTag RangeRows}
    deriving (Show, Eq)

newtype Expressions = Expressions [[Abacus]] deriving (Show, Eq)
newtype Carriage = Carriage Int deriving (Show, Eq)

newtype StartLine = StartLine Int deriving (Show, Eq)
newtype LengthExpr = LengthExpr Int deriving (Show, Eq)
newtype QuantityQuestion = QuantityQuestion Int deriving (Show, Eq)
data Theme = ThemeVoid | Merely | Brother | Friend deriving (Show, Eq, Ord)
data RangeRows = RangeRows Int Int deriving (Show, Eq)


instance Enum Theme where
    toEnum 1 = Merely
    toEnum 2 = Brother
    toEnum 3 = Friend
    toEnum _ = ThemeVoid

    fromEnum ThemeVoid = 0
    fromEnum Merely = 1
    fromEnum Brother = 2
    fromEnum Friend = 3

instance Enum StartLine where
    toEnum 0 = initStartLine
    toEnum _ = initStartLine
    fromEnum (StartLine _) = 0

instance Enum LengthExpr where
    toEnum 0 = initLengthExpr
    toEnum _ = initLengthExpr
    fromEnum (LengthExpr _) = 0

instance Enum QuantityQuestion where
    toEnum 0 = initQuantityQuestion
    toEnum _ = initQuantityQuestion
    fromEnum (QuantityQuestion _) = 0

instance Enum RangeRows where
    toEnum 0 = initRangeRows
    toEnum _ = initRangeRows
    fromEnum (RangeRows _ _) = 0


instance EnumArgs StartLine where
    toEnumArgs n (StartLine {})
        | n == 1 = True
        | otherwise = False
    
    fromEnumArgs (StartLine {}) = 1

instance EnumArgs LengthExpr where
    toEnumArgs n (LengthExpr {})
        | n == 1 = True
        | otherwise = False
    
    fromEnumArgs (LengthExpr {}) = 1

instance EnumArgs QuantityQuestion where
    toEnumArgs n q@(QuantityQuestion {})
        | n == 1 = True
        | otherwise = False
    
    fromEnumArgs (QuantityQuestion {}) = 1

instance EnumArgs Theme where
    toEnumArgs _ _ = False
    fromEnumArgs _ = 0

instance EnumArgs RangeRows where
    toEnumArgs n (RangeRows {})
        | n > 0 && n <= 2 = True
        | otherwise = False
    
    fromEnumArgs (RangeRows {}) = 2


instance Enum EditApp where
    toEnum 0 = EditAppVoid
    toEnum 1 = EditExpressions (Expressions [])
    toEnum 2 = EditRandom (mkStdGen 0)
    toEnum 3 = EditCarriage (Carriage 0)
    toEnum _ = EditAppVoid

    fromEnum EditAppVoid = 0
    fromEnum (EditExpressions _) = 1
    fromEnum (EditRandom _) = 2
    fromEnum (EditCarriage _) = 3

instance Enum EditSet where
    toEnum 0 = EditSetVoid
    toEnum 1 = EditStartLine (Left TypeVoid)
    toEnum 2 = EditLengthExpr (Left TypeVoid)
    toEnum 3 = EditQuantityQuestion (Left TypeVoid)
    toEnum 4 = EditTheme (Left TypeVoid)
    toEnum 5 = EditRangeRows False Horizontal (Left TypeVoid)
    toEnum _ = EditSetVoid

    fromEnum EditSetVoid = 0
    fromEnum (EditStartLine _) = 1
    fromEnum (EditLengthExpr _) = 2
    fromEnum (EditQuantityQuestion _) = 3
    fromEnum (EditTheme _) = 4
    fromEnum (EditRangeRows _ _ _) = 5


instance EnumArgs EditSet where
    toEnumArgs n set
        | n > 0 && n <= numSet = True
        | otherwise = False

        where numSet = fromEnumArgs set

    fromEnumArgs EditSetVoid = 0
    fromEnumArgs (EditStartLine _) = 1
    fromEnumArgs (EditLengthExpr _) = 1
    fromEnumArgs (EditQuantityQuestion _) = 1
    fromEnumArgs (EditTheme _) = 1
    fromEnumArgs (EditRangeRows _ _ _) = 3



initStartLine = StartLine 0
initLengthExpr = LengthExpr 0
initQuantityQuestion = QuantityQuestion 0
initTheme = ThemeVoid
initRangeRows = RangeRows 0 0

isHoriz :: Direct -> Bool
isHoriz Horizontal = True
isHoriz _ = False

isVert :: Direct -> Bool
isVert Vertical = True
isVert _ = False
