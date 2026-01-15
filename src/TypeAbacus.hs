{-# LANGUAGE FlexibleInstances, DuplicateRecordFields, StrictData #-}

module TypeAbacus where

infixr 6 :#*
infixr 6 :#+


data Third a b
    = ThirdNothing
    | ThirdLeft a
    | ThirdRight b
    deriving (Show, Eq, Ord)

data Abacus
    = Abacus [RowAbacus]
    | Plus
    | Minus
    deriving (Show, Eq)

data RowAbacus = RowAbacus [Done] Bool deriving Show

data Done = Done deriving (Show, Eq, Ord)

--newtype Context a = Context a deriving (Show, Eq, Ord) 

data Direct = Horizontal | Vertical deriving (Show, Eq, Ord)

instance Eq RowAbacus where
    RowAbacus leftLower leftUpper == RowAbacus rightLower rightUpper
        | (leftLower == rightLower) && (leftUpper == rightUpper) = True
        | otherwise = False

    leftRow /= rightRow = not $ leftRow == rightRow


data Expr
    = Expr {operator :: Bool}
    | ExprBro {operator :: Bool, ifPlus :: Bool} -- Оператор; Для условия функции

data TypeTag
    = TypeVoid
    | TypeBool Bool
    | TypeInt Int
    | TypeInteger Integer
    | TypeDouble Double
    | TypeString String

    | TypeMaybe (Maybe TypeTag)
    | TypeEither (Either TypeTag TypeTag)
    | TypeList [TypeTag]
    
    | TypeNot TypeTag
    | TypeComp TypeTag

    | TypeTag :#* TypeTag
    | TypeTag :#+ TypeTag
    deriving (Show, Eq, Ord)

instance Num TypeTag where
    TypeInt li + TypeInt ri = TypeInt $ li + ri
    TypeInteger li + TypeInteger ri = TypeInteger $ li + ri
    TypeDouble ld + TypeDouble rd = TypeDouble $ ld + rd

    _ + _ = TypeVoid

    TypeInt li - TypeInt ri = TypeInt $ li - ri
    TypeInteger li - TypeInteger ri = TypeInteger $ li - ri
    TypeDouble ld - TypeDouble rd = TypeDouble $ ld - rd

    _ - _ = TypeVoid

instance Enum TypeTag where
    succ (TypeBool b) = TypeBool $ succ b
    succ (TypeInt i) = TypeInt $ succ i
    succ (TypeInteger i) = TypeInteger $ succ i
    succ (TypeDouble d) = TypeDouble $ succ d

    pred (TypeBool b) = TypeBool $ pred b
    pred (TypeInt i) = TypeInt $ pred i
    pred (TypeInteger i) = TypeInteger $ pred i
    pred (TypeDouble d) = TypeDouble $ pred d

class EnumArgs a where
    toEnumArgs :: Int -> a -> Bool 
    fromEnumArgs :: a -> Int



isThirdJust :: Third a b -> Bool
isThirdJust ThirdNothing = False
isThirdJust _ = True

isThirdRight :: Third a b -> Bool
isThirdRight (ThirdRight _) = True
isThirdRight _ = False

fromThirdLeft :: Third a b -> a
fromThirdLeft (ThirdLeft a) = a

fromThirdRight :: Third a b -> b
fromThirdRight (ThirdRight b) = b
