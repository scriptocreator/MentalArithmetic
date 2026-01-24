{-# LANGUAGE FlexibleInstances, DuplicateRecordFields, StrictData #-}

module TypeAbacus where

import qualified Data.Text as T

infixr 6 :#*
infixr 6 :#+


data UnitEither a
    = UnitLeft {unit :: a}
    | UnitRight {unit :: a}
    deriving (Show, Eq, Ord)

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

data Graph a b
    = GraphElement {name :: a, element :: b}
    | GraphHorizontal {name :: a, graph :: [Graph a b]}
    | GraphVertical {name :: a, graph :: [Graph a b]}
    | GraphMarker (Graph a b)
    deriving Show

data Number a
    = NumberNatural {number :: a}
    | NumberNegative {number :: a}
    deriving (Show, Eq, Ord)

instance Eq RowAbacus where
    RowAbacus leftLower leftUpper == RowAbacus rightLower rightUpper
        | (leftLower == rightLower) && (leftUpper == rightUpper) = True
        | otherwise = False

    leftRow /= rightRow = not $ leftRow == rightRow


data Expr
    = Expr {operator :: Bool}
    | ExprBro {operator :: Bool, ifPlus :: Bool} -- Оператор; Для условия функции

data TypeTag = Sum0
    | Sum1 | Sum2 | Sum3 | Sum4 | Sum5 | Sum6 | Sum7 | Sum8 | Sum9 | Sum10
    | Sum11 | Sum12 | Sum13 | Sum14 | Sum15 | Sum16 | Sum17 | Sum18 | Sum19 | Sum20
    | Sum21 | Sum22 | Sum23 | Sum24 | Sum25 | Sum26 | Sum27 | Sum28 | Sum29 | Sum30
    | Sum31 | Sum32 | Sum33 | Sum34 | Sum35 | Sum36 | Sum37 | Sum38 | Sum39 | Sum40
    | Sum41 | Sum42 | Sum43 | Sum44 | Sum45 | Sum46 | Sum47 | Sum48 | Sum49 | Sum50
    | Sum51 | Sum52 | Sum53 | Sum54 | Sum55 | Sum56 | Sum57 | Sum58 | Sum59 | Sum60
    | Sum61 | Sum62 | Sum63 | Sum64 | Sum65 | Sum66 | Sum67 | Sum68 | Sum69 | Sum70
    | Sum71 | Sum72 | Sum73 | Sum74 | Sum75 | Sum76 | Sum77 | Sum78 | Sum79 | Sum80
    | Sum81 | Sum82 | Sum83 | Sum84 | Sum85 | Sum86 | Sum87 | Sum88 | Sum89 | Sum90
    | Sum91 | Sum92 | Sum93 | Sum94 | Sum95 | Sum96 | Sum97 | Sum98 | Sum99 | Sum100

    | TypeNegative

    | TypeVoid
    | TypeBool {bool :: Bool}
    | TypeInt {int :: Int}
    | TypeInteger {integer :: Integer}
    | TypeDouble {double :: Double}
    | TypeString {string :: String}
    | TypeText {text :: T.Text}

    | TypeMaybe {maybe :: Maybe TypeTag}
    | TypeEither {either :: Either TypeTag TypeTag}
    | TypeList {list :: [TypeTag]}

    | TypeNot TypeTag
    | TypeVar T.Text TypeTag

    | TypeComp {comp :: TypeTag}
    | TypePair {left :: TypeTag, right :: TypeTag}
    | TypeTrinity {left :: TypeTag, center :: TypeTag, right :: TypeTag}
    
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
    
    toEnum 0 = Sum0
    toEnum 1 = Sum1; toEnum 2 = Sum2; toEnum 3 = Sum3; toEnum 4 = Sum4; toEnum 5 = Sum5
    toEnum 6 = Sum6; toEnum 7 = Sum7; toEnum 8 = Sum8; toEnum 9 = Sum9; toEnum 10 = Sum10

    toEnum 11 = Sum11; toEnum 12 = Sum12; toEnum 13 = Sum13; toEnum 14 = Sum14; toEnum 15 = Sum15
    toEnum 16 = Sum16; toEnum 17 = Sum17; toEnum 18 = Sum18; toEnum 19 = Sum19; toEnum 20 = Sum20

    toEnum 21 = Sum21; toEnum 22 = Sum22; toEnum 23 = Sum23; toEnum 24 = Sum24; toEnum 25 = Sum25
    toEnum 26 = Sum26; toEnum 27 = Sum27; toEnum 28 = Sum28; toEnum 29 = Sum29; toEnum 30 = Sum30
    
    toEnum 31 = Sum31; toEnum 32 = Sum32; toEnum 33 = Sum33; toEnum 34 = Sum34; toEnum 35 = Sum35
    toEnum 36 = Sum36; toEnum 37 = Sum37; toEnum 38 = Sum38; toEnum 39 = Sum39; toEnum 40 = Sum40

    toEnum 41 = Sum41; toEnum 42 = Sum42; toEnum 43 = Sum43; toEnum 44 = Sum44; toEnum 45 = Sum45
    toEnum 46 = Sum46; toEnum 47 = Sum47; toEnum 48 = Sum48; toEnum 49 = Sum49; toEnum 50 = Sum50

    toEnum 51 = Sum51; toEnum 52 = Sum52; toEnum 53 = Sum53; toEnum 54 = Sum54; toEnum 55 = Sum55
    toEnum 56 = Sum56; toEnum 57 = Sum57; toEnum 58 = Sum58; toEnum 59 = Sum59; toEnum 60 = Sum60

    toEnum 61 = Sum61; toEnum 62 = Sum62; toEnum 63 = Sum63; toEnum 64 = Sum64; toEnum 65 = Sum65
    toEnum 66 = Sum66; toEnum 67 = Sum67; toEnum 68 = Sum68; toEnum 69 = Sum69; toEnum 70 = Sum70

    toEnum 71 = Sum71; toEnum 72 = Sum72; toEnum 73 = Sum73; toEnum 74 = Sum74; toEnum 75 = Sum75
    toEnum 76 = Sum76; toEnum 77 = Sum77; toEnum 78 = Sum78; toEnum 79 = Sum79; toEnum 80 = Sum80

    toEnum 81 = Sum81; toEnum 82 = Sum82; toEnum 83 = Sum83; toEnum 84 = Sum84; toEnum 85 = Sum85
    toEnum 86 = Sum86; toEnum 87 = Sum87; toEnum 88 = Sum88; toEnum 89 = Sum89; toEnum 90 = Sum90

    toEnum 91 = Sum91; toEnum 92 = Sum92; toEnum 93 = Sum93; toEnum 94 = Sum94; toEnum 95 = Sum95
    toEnum 96 = Sum96; toEnum 97 = Sum97; toEnum 98 = Sum98; toEnum 99 = Sum99; toEnum 100 = Sum100
    toEnum n = TypeVoid


    fromEnum Sum0 = 0
    fromEnum Sum1 = 1; fromEnum Sum2 = 2; fromEnum Sum3 = 3; fromEnum Sum4 = 4; fromEnum Sum5 = 5
    fromEnum Sum6 = 6; fromEnum Sum7 = 7; fromEnum Sum8 = 8; fromEnum Sum9 = 9; fromEnum Sum10 = 10

    fromEnum Sum11 = 11; fromEnum Sum12 = 12; fromEnum Sum13 = 13; fromEnum Sum14 = 14; fromEnum Sum15 = 15
    fromEnum Sum16 = 16; fromEnum Sum17 = 17; fromEnum Sum18 = 18; fromEnum Sum19 = 19; fromEnum Sum20 = 20

    fromEnum Sum21 = 21; fromEnum Sum22 = 22; fromEnum Sum23 = 23; fromEnum Sum24 = 24; fromEnum Sum25 = 25
    fromEnum Sum26 = 26; fromEnum Sum27 = 27; fromEnum Sum28 = 28; fromEnum Sum29 = 29; fromEnum Sum30 = 30

    fromEnum Sum31 = 31; fromEnum Sum32 = 32; fromEnum Sum33 = 33; fromEnum Sum34 = 34; fromEnum Sum35 = 35
    fromEnum Sum36 = 36; fromEnum Sum37 = 37; fromEnum Sum38 = 38; fromEnum Sum39 = 39; fromEnum Sum40 = 40

    fromEnum Sum41 = 41; fromEnum Sum42 = 42; fromEnum Sum43 = 43; fromEnum Sum44 = 44; fromEnum Sum45 = 45
    fromEnum Sum46 = 46; fromEnum Sum47 = 47; fromEnum Sum48 = 48; fromEnum Sum49 = 49; fromEnum Sum50 = 50

    fromEnum Sum51 = 51; fromEnum Sum52 = 52; fromEnum Sum53 = 53; fromEnum Sum54 = 54; fromEnum Sum55 = 55
    fromEnum Sum56 = 56; fromEnum Sum57 = 57; fromEnum Sum58 = 58; fromEnum Sum59 = 59; fromEnum Sum60 = 60

    fromEnum Sum61 = 61; fromEnum Sum62 = 62; fromEnum Sum63 = 63; fromEnum Sum64 = 64; fromEnum Sum65 = 65
    fromEnum Sum66 = 66; fromEnum Sum67 = 67; fromEnum Sum68 = 68; fromEnum Sum69 = 69; fromEnum Sum70 = 70

    fromEnum Sum71 = 71; fromEnum Sum72 = 72; fromEnum Sum73 = 73; fromEnum Sum74 = 74; fromEnum Sum75 = 75
    fromEnum Sum76 = 76; fromEnum Sum77 = 77; fromEnum Sum78 = 78; fromEnum Sum79 = 79; fromEnum Sum80 = 80

    fromEnum Sum81 = 81; fromEnum Sum82 = 82; fromEnum Sum83 = 83; fromEnum Sum84 = 84; fromEnum Sum85 = 85
    fromEnum Sum86 = 86; fromEnum Sum87 = 87; fromEnum Sum88 = 88; fromEnum Sum89 = 89; fromEnum Sum90 = 90

    fromEnum Sum91 = 91; fromEnum Sum92 = 92; fromEnum Sum93 = 93; fromEnum Sum94 = 94; fromEnum Sum95 = 95
    fromEnum Sum96 = 96; fromEnum Sum97 = 97; fromEnum Sum98 = 98; fromEnum Sum99 = 99; fromEnum Sum100 = 100
    fromEnum n = 102

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

isNatural :: Number a -> Bool
isNatural (NumberNatural _) = True
isNatural _ = False

isNegative :: Number a -> Bool
isNegative (NumberNegative _) = True
isNegative _ = False
