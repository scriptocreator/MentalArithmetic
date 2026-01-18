{-# LANGUAGE Strict #-}

module ImperativeAbacus where

import TypeAbacus
import TypeImperativeAbacus
import PureFunctions ( sortTypeList, prodTypeToList, sumTypeToList, tailType )

import System.Random
import System.Random.Stateful
import Data.Maybe (fromJust, isJust)
import Graphics.Gloss.Interface.Pure.Game
import Data.Either



funcGetEdit :: EditSet -> TypeTag
funcGetEdit set = case set of
    (EditStartLine _) -> fromLeft TypeVoid $ editStart set
    (EditLengthExpr _) -> fromLeft TypeVoid $ editExpr set
    (EditQuantityQuestion _) -> fromLeft TypeVoid $ editQuest set
    (EditTheme _) -> fromLeft TypeVoid $ editTheme set
    (EditRangeRows _ _ _) -> fromLeft TypeVoid $ editRange set


funcEdit :: EditSet -> TypeTag -> EditSet
funcEdit set funcType = case set of
    (EditStartLine _) -> set {editStart = Left funcType}
    (EditLengthExpr _) -> set {editExpr = Left funcType}
    (EditQuantityQuestion _) -> set {editQuest = Left funcType}
    (EditTheme _) -> set {editTheme = Left funcType}
    (EditRangeRows _ _ _) -> set {editRange = Left funcType}


numInRows :: Int -> [Int]
numInRows num = createRows num $ reverse $ accRows 1 9

    where accRows row rowNum
            | num <= rowNum = [row]
            | otherwise = row : accRows newRow curRowNum

            where curRowNum = exponent rowNum
                  newRow = row * 10

          createRows _ [] = []
          createRows nesNum (row:rows) = nesNum `div` row : createRows newNum rows

            where newNum = nesNum `div` 10
        
          exponent num = num * 10 + num


rowsInNum :: [Int] -> Int
rowsInNum rows = nestedNum $ zip [1,10..] rows
    
    where nestedNum [] = 0
          nestedNum ((level, var):rs) = level * var + nestedNum rs


keyInBool :: SpecialKey -> Bool
keyInBool KeyDown = True
keyInBool KeyRight = True
keyInBool _ = False


inliningMarker :: [TypeTag] -> [TypeTag]
inliningMarker [] = [TypeBool True]
inliningMarker (TypeBool _:b) = TypeBool True : b
inliningMarker (a:b) = a : inliningMarker b


inliningNum :: TypeTag -> [TypeTag] -> [TypeTag]
inliningNum num [] = num : TypeBool True : []
inliningNum num (TypeBool _:b) = num : TypeBool True : b
inliningNum num (a:b) = a : inliningNum num b


figureCharToType :: Char -> TypeTag
figureCharToType '1' = Sum1
figureCharToType '2' = Sum2
figureCharToType '3' = Sum3
figureCharToType '4' = Sum4
figureCharToType '5' = Sum5
figureCharToType '6' = Sum6
figureCharToType '7' = Sum7
figureCharToType '8' = Sum8
figureCharToType '9' = Sum9
figureCharToType '0' = Sum0
figureCharToType _ = TypeVoid


figureIntToType :: Int -> TypeTag
figureIntToType n | n >= 0 && n < 10 = toEnum n
figureIntToType _ = TypeVoid


typeToListInt :: [TypeTag] -> [Int]
typeToListInt [] = []
typeToListInt (t:ts)
    | curFigure < 10 = curFigure : typeToListInt ts
    | otherwise = typeToListInt ts

    where curFigure = fromEnum t


mathDirKey :: Direct -> SpecialKey -> Ordering
mathDirKey Horizontal KeyUp = LT
mathDirKey Horizontal KeyDown = GT
mathDirKey Horizontal _ = EQ
mathDirKey Vertical KeyLeft = LT
mathDirKey Vertical KeyRight = GT
mathDirKey Vertical _ = EQ


enumArgsSet :: EditSet -> Int
enumArgsSet (EditStartLine (Right e)) = fromEnumArgs e
enumArgsSet (EditLengthExpr (Right e)) = fromEnumArgs e
enumArgsSet (EditQuantityQuestion (Right e)) = fromEnumArgs e
enumArgsSet (EditTheme (Right e)) = fromEnumArgs e
enumArgsSet (EditRangeRows _ _ (Right e)) = fromEnumArgs e


initRightSet :: EditSet -> EditSet
initRightSet (EditStartLine _) = EditStartLine $ Right initStartLine
initRightSet (EditLengthExpr _) = EditLengthExpr $ Right initLengthExpr
initRightSet (EditQuantityQuestion _) = EditQuantityQuestion $ Right initQuantityQuestion
initRightSet (EditTheme _) = EditTheme $ Right initTheme
initRightSet (EditRangeRows _ _ _) = EditRangeRows False Horizontal $ Right initRangeRows


setToType :: EditSet -> EditSet
setToType (EditStartLine (Right (StartLine start))) = EditStartLine $ Left $ TypeInt start
setToType (EditLengthExpr (Right (LengthExpr lenExpr))) = EditLengthExpr $ Left $ TypeInt lenExpr
setToType (EditQuantityQuestion (Right (QuantityQuestion quant))) = EditQuantityQuestion $ Left $ TypeInt quant
setToType (EditTheme (Right t)) = EditTheme $ Left $ TypeInt $ fromEnum t
setToType (EditRangeRows nes dir (Right (RangeRows firstI secondI)))
    =  EditRangeRows nes dir $ Left $
    TypeList [TypePair (TypeInt 0) (TypeList (funcFromI firstI)), TypePair (TypeInt 1) (TypeList (funcFromI secondI))]
setToType typeTag = typeTag


typeToSet :: EditSet -> Maybe EditSet
typeToSet (EditStartLine (Left (TypeInt start))) = Just $ EditStartLine $ Right $ StartLine start
typeToSet (EditLengthExpr (Left (TypeInt lenExpr))) = Just $ EditLengthExpr $ Right $ LengthExpr lenExpr
typeToSet (EditQuantityQuestion (Left (TypeInt quant))) = Just $ EditQuantityQuestion $ Right $ QuantityQuestion quant
typeToSet (EditTheme (Left (TypeInt i))) = Just $ EditTheme $ Right $ toEnum i
typeToSet (EditRangeRows nes dir (Left (TypeList myType@[TypePair (TypeInt _) (TypeList _), TypePair (TypeInt _) (TypeList _)])))
    = if length typeList == fromEnumArgs initRangeRows
        then Just $ EditRangeRows nes dir $ Right $ RangeRows (funcToI firstI) (funcToI secondI)
        else Nothing

    where typeList = fmap right $ sortTypeList myType
          [TypeList firstI, TypeList secondI] = typeList

typeToSet right | isRightInSet right = Just right
typeToSet _ = Nothing


funcFromI :: Int -> [TypeTag]
funcFromI i = fmap figureIntToType $ numInRows i

funcToI :: [TypeTag] -> Int
funcToI tl = rowsInNum $ typeToListInt tl


isRightInSet :: EditSet -> Bool
isRightInSet (EditStartLine (Right _)) = True
isRightInSet (EditLengthExpr (Right _)) = True
isRightInSet (EditQuantityQuestion (Right _)) = True
isRightInSet (EditTheme (Right _)) = True
isRightInSet (EditRangeRows _ _ (Right _)) = True
isRightInSet _ = False


mathElemEditSet :: EditSet -> EditSet -> Bool
mathElemEditSet (EditStartLine {}) (EditStartLine {}) = True
mathElemEditSet (EditLengthExpr {}) (EditLengthExpr {}) = True
mathElemEditSet (EditQuantityQuestion {}) (EditQuantityQuestion {}) = True
mathElemEditSet (EditTheme {}) (EditTheme {}) = True
mathElemEditSet (EditRangeRows {}) (EditRangeRows {}) = True
mathElemEditSet _ _ = False


sortEditApp :: [EditApp] -> Maybe [EditApp]
sortEditApp = nestedSort Nothing Nothing Nothing

    where nestedSort (Just expr) (Just rand) (Just carr) [] = Just [expr, rand, carr]
          nestedSort expr rand carr (x@(EditExpressions {}):xs) = nestedSort (return x) rand carr xs
          nestedSort expr rand carr (x@(EditRandom {}):xs)      = nestedSort expr (return x) carr xs
          nestedSort expr rand carr (x@(EditCarriage {}):xs)    = nestedSort expr rand (return x) xs
          nestedSort _ _ _ _ = Nothing


sortEditSet :: [EditSet] -> Maybe [EditSet]
sortEditSet = nestedSort Nothing Nothing Nothing Nothing Nothing

    where nestedSort (Just start) (Just lenExpr) (Just quant) (Just theme) (Just range) [] = return [start, lenExpr, quant, theme, range, range]
          nestedSort start lenExpr quant theme range (x@(EditStartLine {}):xs)        = nestedSort (return x) lenExpr quant theme range xs
          nestedSort start lenExpr quant theme range (x@(EditLengthExpr {}):xs)       = nestedSort start (return x) quant theme range xs
          nestedSort start lenExpr quant theme range (x@(EditQuantityQuestion {}):xs) = nestedSort start lenExpr (return x) theme range xs
          nestedSort start lenExpr quant theme range (x@(EditTheme {}):xs)            = nestedSort start lenExpr quant (return x) range xs
          nestedSort start lenExpr quant theme range (x@(EditRangeRows {}):xs)        = nestedSort start lenExpr quant theme (return x) xs
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
abacusInNum abacus = internalNum $ zip [1,10..] abacus

    where internalNum [] = 0
          internalNum ((row, RowAbacus lower upper):abacusis) = let numUpper = if upper then 5 * row else 0
            in length lower * row + numUpper + internalNum abacusis


exprAbacusInList :: [Abacus] -> [String]
exprAbacusInList [] = []
exprAbacusInList (Plus:as) = " + " : exprAbacusInList as
exprAbacusInList (Minus:as) = " - " : exprAbacusInList as
exprAbacusInList (Abacus abacus:as) = show (abacusInNum abacus) : exprAbacusInList as
