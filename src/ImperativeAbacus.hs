{-# LANGUAGE Strict, StrictData #-}

module ImperativeAbacus where

import TypeAbacus

import System.Random
import System.Random.Stateful

infixr 6 :\^/
infixr 6 :#*


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
    | EditStartLine (Either Type StartLine)
    | EditLengthExpr (Either Type LengthExpr)
    | EditQuantityQuestion (Either Type QuantityQuestion)
    | EditTheme (Either Type Theme)
    | EditRangeRows (Either Type RangeRows)
    deriving (Show, Eq)

newtype Expressions = Expressions [[Abacus]] deriving (Show, Eq)
newtype Carriage = Carriage Int deriving (Show, Eq)

newtype StartLine = StartLine Int deriving (Show, Eq)
newtype LengthExpr = LengthExpr Int deriving (Show, Eq)
newtype QuantityQuestion = QuantityQuestion Int deriving (Show, Eq)
data Theme = {-ThemeVoid |-} Merely | Brother | Friend deriving (Show, Eq, Ord)
data RangeRows = RangeRows Int Int deriving (Show, Eq)

data Type
    = TypeVoid
    | TypeBool Bool
    | TypeInt Int
    | TypeDouble Double
    | TypeString String
    
    | Type :#* Type
    deriving (Show, Eq, Ord)

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
  toEnum 5 = EditRangeRows (Left TypeVoid)
  toEnum _ = EditSetVoid
  
  fromEnum EditSetVoid = 0
  fromEnum (EditStartLine _) = 1
  fromEnum (EditLengthExpr _) = 2
  fromEnum (EditQuantityQuestion _) = 3
  fromEnum (EditTheme _) = 4
  fromEnum (EditRangeRows _) = 5


typeToSet :: EditSet -> EditSet
typeToSet (EditStartLine (Left (TypeInt start))) = EditStartLine $ Right $ StartLine start
typeToSet (EditLengthExpr (Left (TypeInt lenExpr))) = EditLengthExpr $ Right $ LengthExpr lenExpr
typeToSet (EditQuantityQuestion (Left (TypeInt quant))) = EditQuantityQuestion $ Right $ QuantityQuestion quant
typeToSet (EditTheme (Left (TypeInt 0))) = EditTheme $ Right $ Merely
typeToSet (EditTheme (Left (TypeInt 1))) = EditTheme $ Right $ Brother
typeToSet (EditTheme (Left (TypeInt 2))) = EditTheme $ Right $ Friend
typeToSet (EditRangeRows (Left (TypeInt a :#* TypeInt b))) = EditRangeRows $ Right $ RangeRows a b

typeToSet right = right

mathElemEditSet :: EditSet -> EditSet -> Bool
mathElemEditSet (EditStartLine _) (EditStartLine _) = True
mathElemEditSet (EditLengthExpr _) (EditLengthExpr _) = True
mathElemEditSet (EditQuantityQuestion _) (EditQuantityQuestion _) = True
mathElemEditSet (EditTheme _) (EditTheme _) = True
mathElemEditSet (EditRangeRows _) (EditRangeRows _) = True
mathElemEditSet _ _ = False

sortEditApp :: [EditApp] -> Maybe [EditApp]
sortEditApp = nestedSort Nothing Nothing Nothing

    where nestedSort (Just expr) (Just rand) (Just carr) [] = Just [expr, rand, carr]
          nestedSort expr rand carr (x@(EditExpressions _):xs) = nestedSort (return x) rand carr xs
          nestedSort expr rand carr (x@(EditRandom _):xs) = nestedSort expr (return x) carr xs
          nestedSort expr rand carr (x@(EditCarriage _):xs) = nestedSort expr rand (return x) xs
          nestedSort _ _ _ _ = Nothing

sortEditSet :: [EditSet] -> Maybe [EditSet]
sortEditSet = nestedSort Nothing Nothing Nothing Nothing Nothing

    where nestedSort (Just start) (Just lenExpr) (Just quant) (Just theme) (Just range) [] = Just [start, lenExpr, quant, theme, range, range]
          nestedSort start lenExpr quant theme range (x@(EditStartLine _):xs) = nestedSort (return x) lenExpr quant theme range xs
          nestedSort start lenExpr quant theme range (x@(EditLengthExpr _):xs) = nestedSort start (return x) quant theme range xs
          nestedSort start lenExpr quant theme range (x@(EditQuantityQuestion _):xs) = nestedSort start lenExpr (return x) theme range xs
          nestedSort start lenExpr quant theme range (x@(EditTheme _):xs) = nestedSort start lenExpr quant (return x) range xs
          nestedSort start lenExpr quant theme range (x@(EditRangeRows _):xs) = nestedSort start lenExpr quant theme (return x) xs
          nestedSort _ _ _ _ _ _ = Nothing

powerInAbacus :: StdGen -> (Int, Int) -> (RowAbacus, StdGen)
powerInAbacus gen (numLower, numUpper) = (RowAbacus newLower newUpper, finalGen)

    where (randomLower, firstGen) = randomR (0, numLower) gen
          newLower = replicate randomLower Done

          (newUpper, finalGen) = if numUpper == 0
            then (False, firstGen)
            else let (num, secondGen) = randomR (0, numUpper) firstGen
                     curUpper = (num /= 0)
              in (curUpper, secondGen)
--powerInAbacus (numLower, numUpper) = RowAbacus (take numLower $ repeat Done) (if numUpper == 0 then False else True)

abacusInNum :: [RowAbacus] -> Int
abacusInNum abacus = internalNum $ zip [1,2..] abacus

    where internalNum [] = 0
          internalNum ((row, RowAbacus lower upper):abacusis) = let numUpper = if upper then 5 * row else 0
            in length lower * row + numUpper + internalNum abacusis

exprAbacusInList :: [Abacus] -> [String]
exprAbacusInList [] = []
exprAbacusInList (Plus:as) = " + " : exprAbacusInList as
exprAbacusInList (Minus:as) = " - " : exprAbacusInList as
exprAbacusInList (Abacus abacus:as) = show (abacusInNum abacus) : exprAbacusInList as
