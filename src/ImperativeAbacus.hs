-- {-# LANGUAGE Strict #-}

module ImperativeAbacus where

import TypeAbacus
import TypeImperativeAbacus
import PureFunctions ( sortTypeList, prodTypeToList, sumTypeToList, tailType, isTypeBool )

import System.Random
import System.Random.Stateful
import Data.Maybe (fromJust, isJust)
import Graphics.Gloss.Interface.Pure.Game
import Data.Either ( fromLeft, fromRight )
import Text.Printf (printf)



setToGraph :: EditSet -> Graph String
setToGraph set@(EditTheme {}) = (GraphElement . show . fromRight ThemeVoid . editTheme . fromJust) $ typeToSet set
setToGraph set@(EditRangeRows {}) = 
    let dir = direct set
    in if isHoriz dir
        then GraphHorizontal stringsArgs
        else GraphVertical stringsArgs
    
    where typeList = (fmap right . sortTypeList . list) $ funcGetEdit (setToType set)
          stringsArgs = fmap (GraphElement . typeToString . list) typeList

setToGraph set = (GraphElement . typeToString . list) $ funcGetEdit (setToType set)


typeToString :: [TypeTag] -> String
typeToString [] = []
typeToString (t:ts)
    | fromEnum t < 10 = (show . fromEnum) t ++ typeToString ts
    | isTypeBool t && bool t = '|' : typeToString ts
    | otherwise = typeToString ts


sortSetList :: [EditSet] -> [EditSet]
sortSetList = nestedSort []

    where nestedSort :: [(Int, EditSet)] -> [EditSet] -> [EditSet]
          nestedSort sortList [] = fmap snd sortList
          nestedSort sortList (s:ss) = 
            let sortS = (fromEnum s, s)
            in nestedSort (putSetList sortS sortList) ss


putSetList :: (Int, EditSet) -> [(Int, EditSet)] -> [(Int, EditSet)]
putSetList s [] = [s]
putSetList s (sel:ss)
    | fst s == fst sel
        = error $ printf "Error ImperativeAbacus №1: Обнаружена ещё одна переменная под аргумент конструктора – «%s» и «%s»"
                         (show arg1)
                         (show arg2)
    | fst s < fst sel = s : sel : ss
    | otherwise = sel : putSetList s ss

    where arg1 = snd s
          arg2 = snd sel 


funcGetEdit :: EditSet -> TypeTag
funcGetEdit set = case set of
    (EditStartLine {}) -> fromLeft TypeVoid $ editStart set
    (EditLengthExpr {}) -> fromLeft TypeVoid $ editExpr set
    (EditQuantityQuestion {}) -> fromLeft TypeVoid $ editQuest set
    (EditTheme {}) -> fromLeft TypeVoid $ editTheme set
    (EditRangeRows {}) -> fromLeft TypeVoid $ editRange set


funcPutEdit :: EditSet -> TypeTag -> EditSet
funcPutEdit set funcType = case set of
    (EditStartLine {}) -> set {editStart = Left funcType}
    (EditLengthExpr {}) -> set {editExpr = Left funcType}
    (EditQuantityQuestion {}) -> set {editQuest = Left funcType}
    (EditTheme {}) -> set {editTheme = Left funcType}
    (EditRangeRows {}) -> set {editRange = Left funcType}


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
inliningNum num [] = [num, TypeBool True]
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


typeToFigureInt :: [TypeTag] -> [Int]
typeToFigureInt [] = []
typeToFigureInt (t:ts)
    | curFigure < 10 = curFigure : typeToFigureInt ts
    | otherwise = typeToFigureInt ts

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


enumSet :: EditSet -> Int
enumSet (EditStartLine (Right e)) = fromEnum e
enumSet (EditLengthExpr (Right e)) = fromEnum e
enumSet (EditQuantityQuestion (Right e)) = fromEnum e
enumSet (EditTheme (Right e)) = fromEnum e
enumSet (EditRangeRows _ _ (Right e)) = fromEnum e


initRightSet :: EditSet -> EditSet
initRightSet (EditStartLine {}) = EditStartLine $ Right initStartLine
initRightSet (EditLengthExpr {}) = EditLengthExpr $ Right initLengthExpr
initRightSet (EditQuantityQuestion {}) = EditQuantityQuestion $ Right initQuantityQuestion
initRightSet (EditTheme {}) = EditTheme $ Right initTheme
initRightSet (EditRangeRows {}) = EditRangeRows False Horizontal $ Right initRangeRows


setToType :: EditSet -> EditSet
setToType (EditStartLine (Right (StartLine start))) = EditStartLine $ Left $ TypeList $ funcFromI start
setToType (EditLengthExpr (Right (LengthExpr lenExpr))) = EditLengthExpr $ Left $ TypeList $ funcFromI lenExpr
setToType (EditQuantityQuestion (Right (QuantityQuestion quant))) = EditQuantityQuestion $ Left $ TypeList $ funcFromI quant
setToType (EditTheme (Right t)) = EditTheme $ Left $ TypeInt $ fromEnum t
setToType (EditRangeRows nes dir (Right (RangeRows firstI secondI)))
    =  EditRangeRows nes dir $ Left $
    TypeList [TypePair (TypeInt 0) (TypeList (funcFromI firstI)), TypePair (TypeInt 1) (TypeList (funcFromI secondI))]
setToType typeTag = typeTag


typeToSet :: EditSet -> Maybe EditSet
typeToSet (EditStartLine (Left (TypeList start))) = Just $ EditStartLine $ Right $ StartLine $ funcToI start
typeToSet (EditLengthExpr (Left (TypeList lenExpr))) = Just $ EditLengthExpr $ Right $ LengthExpr $ funcToI lenExpr
typeToSet (EditQuantityQuestion (Left (TypeList quant))) = Just $ EditQuantityQuestion $ Right $ QuantityQuestion $ funcToI quant
typeToSet (EditTheme (Left (TypeInt i))) = Just $ EditTheme $ Right $ toEnum i
typeToSet (EditRangeRows nes dir (Left (TypeList myType@[TypePair (TypeInt _) (TypeList _), TypePair (TypeInt _) (TypeList _)])))
    = if length typeList == fromEnumArgs initRangeRows
        then Just $ EditRangeRows nes dir $ Right $ RangeRows (funcToI firstI) (funcToI secondI)
        else Nothing

    where typeList = (fmap right . sortTypeList) myType
          [TypeList firstI, TypeList secondI] = typeList

typeToSet r | isRightInSet r = Just r
typeToSet _ = Nothing


funcFromI :: Int -> [TypeTag]
funcFromI i = fmap figureIntToType $ numInRows i

funcToI :: [TypeTag] -> Int
funcToI tl = rowsInNum $ typeToFigureInt tl


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

{-
sortEditApp :: [EditApp] -> Maybe [EditApp]
sortEditApp = nestedSort Nothing Nothing Nothing

    where nestedSort (Just expr) (Just rand) (Just carr) [] = Just [expr, rand, carr]
          nestedSort expr rand carr (x@(EditExpressions {}):xs) = nestedSort (return x) rand carr xs
          nestedSort expr rand carr (x@(EditRandom {}):xs)      = nestedSort expr (return x) carr xs
          nestedSort expr rand carr (x@(EditCarriage {}):xs)    = nestedSort expr rand (return x) xs
          nestedSort _ _ _ _ = Nothing
-}

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

          (newUpper, finalGen) =
            let (num, secondGen) = randomR (0, numUpper) firstGen
                curUpper = num /= 0
            in if numUpper == 0
                then (False, firstGen)
                else (curUpper, secondGen)

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
