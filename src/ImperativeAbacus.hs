-- {-# LANGUAGE Strict #-}

module ImperativeAbacus where

import TypeAbacus
import TypeImperativeAbacus
import PureFunctions ( sortTypeList, prodTypeToList, sumTypeToList, tailType, isTypeBool, funcIf )

import System.Random ( StdGen, Random(randomR) )
import System.Random.Stateful
import Data.Maybe (fromJust, isJust, isNothing)
import Graphics.Gloss.Interface.Pure.Game
import Data.Either ( fromLeft, fromRight )
import Text.Printf (printf)
import Data.List (find)



checkAbacus :: [RowAbacus] -> [RowAbacus] -> Theme -> Abacus -> Bool
checkAbacus abacus curAbacus _ {-Merely-} oper = False
{-
    where curOper = case oper of
            Minus -> let resultAbacus = abacus - curAbacus
                in resultAbacus
            Plus -> 
            expr -> error $ show expr
          abacus curAbacus

checkAbacus abacus curAbacus Brother
checkAbacus abacus curAbacus Friend
-}

setToGraph :: [EditSet] -> [Graph String String]
setToGraph [] = []
setToGraph sets = GraphMarker headS : tailS

    where (headS:tailS) = fmap unitSetToGraph sets


unitSetToGraph :: EditSet -> Graph String String
unitSetToGraph set@(EditTheme {}) =
    let mSet = typeToSet set
        reallySet = fromJust mSet
        name = "Theme"

    in if isNothing mSet
        then (GraphElement name . show) initTheme
        else (GraphElement name . show . fromRight ThemeVoid . editTheme) reallySet

unitSetToGraph set@(EditRangeRows {}) =
    let dir = direct set
        name = nameSet set

    in --funcIf True (GraphElement name (show set)) $
       funcIf (tlSet == TypeVoid) (GraphElement name "") $
       funcIf (isHoriz dir)
              (GraphHorizontal name graphStringsArgs)
              (GraphVertical name graphStringsArgs)

    where tlSet = funcGetEdit (setToType set)
          
          (headTypeList:tailTypeList) = list tlSet
          eitListWithMarker = UnitRight headTypeList : fmap UnitLeft tailTypeList
          sortEitList = sortUnitList () lambdaEitGraph eitListWithMarker
          graphStringsArgs = fmap fromUnit sortEitList

          fromUnit (UnitRight t) = (GraphMarker . funcTypeNumToString . right) t
          fromUnit t = (funcTypeNumToString . right . unit) t

          element = GraphElement "arg"

          funcTypeNumToString TypeVoid = element ""
          funcTypeNumToString tNum@(TypeList l) = (element . typeNumToString . list) tNum

unitSetToGraph set =
    let typeTag = funcGetEdit (setToType set)
        name = nameSet set

    in if typeTag == TypeVoid
        then GraphElement name ""
        else (GraphElement name . typeNumToString . list) typeTag


lambdaEitGraph :: () -> UnitEither TypeTag -> ((), Int)
lambdaEitGraph _ u = ((), int $ left $ unit u)


nameSet :: EditSet -> String
nameSet set =
    let mSet = typeToSet set
        updSet = fromJust mSet
        compSet = head . words . showEditSet
        
    in if isJust mSet
        then compSet updSet
        else compSet $ initRightSet set


typeNumToString :: [TypeTag] -> String
typeNumToString [] = []
typeNumToString (TypeNegative:ts) = '-' : typeNumToString ts
typeNumToString (t:ts)
    | fromEnum t < 10 = (show . fromEnum) t ++ typeNumToString ts
    | isTypeBool t && bool t = '|' : typeNumToString ts
    | otherwise = typeNumToString ts


sortSetGraphList :: [(EditSet, Graph String String)] -> [(EditSet, Graph String String)]
sortSetGraphList = sortUnitList () lambdaGraph

lambdaGraph :: () -> (EditSet, b) -> ((), Int)
lambdaGraph _ tuple = 
    let lambda = (fromEnum :: EditSet -> Int) . fst
    in ((), lambda tuple)


sortEditSet :: [EditSet] -> Maybe [EditSet]
sortEditSet sets =
    let sort = sortUnitList () lambdaEditSet sets
        filtSort = filter (/=EditSetVoid) sort
    in if length filtSort == 5
        then return filtSort
        else Nothing

lambdaEditSet :: () -> EditSet -> ((), Int)
lambdaEditSet () s = ((), fromEnum s)


sortUnitList :: Show a => c -> (c -> a -> (c, Int)) -> [a] -> [a]
sortUnitList c fromA = nestedSort c []

    where nestedSort c sortList [] = fmap snd sortList
          nestedSort c sortList (u:us) =
            let (newC, numU) = fromA c u
                sortU = (numU, u)
            in nestedSort newC (putUnitList sortU sortList) us

          putUnitList s [] = [s]
          putUnitList s (sel:ss)
            | fst s == fst sel
                = error $ printf "Error ImperativeAbacus №1: Обнаружена ещё одна переменная под аргумент конструктора – «%s» и «%s»"
                                 (show arg1)
                                 (show arg2)
            | fst s < fst sel = s : sel : ss
            | otherwise = sel : putUnitList s ss

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


numInRows :: Int -> Number [Int]
numInRows num = if num < 0
    then NumberNegative listNumber
    else NumberNatural listNumber

    where absNum = abs num

          accRows rowSimula numSimula
            | absNum <= numSimula = [tupleRow]
            | otherwise = tupleRow : accRows newRow newNumSimula

            where tupleRow = (newRow, rowSimula)
                  newRow = rowSimula * 10
                  newNumSimula = exponent numSimula

          createRows [] = []
          createRows ((modRow, divRow):rows)
            = absNum `mod` modRow `div` divRow : createRows rows

          exponent sim = sim * 10 + sim

          listNumber = createRows $ reverse $ accRows 1 9


rowsInNum :: Number [Int] -> Int
rowsInNum nRows = if isNegative nRows
    then (-unitNumber)
    else unitNumber

    where rows :: [Int]
          rows = number nRows

          nestedNum :: [(Int, Int)] -> Int
          nestedNum [] = 0
          nestedNum ((level, var):rs) = level * var + nestedNum rs

          unitNumber :: Int
          unitNumber = nestedNum $ zip (iterate (*10) 1) rows


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
figureCharToType '-' = TypeNegative
figureCharToType _ = TypeVoid


figureIntToType :: Number [Int] -> [TypeTag]
figureIntToType nListNum = if isNegative nListNum
    then TypeNegative : listType
    else listType

    where listNum = number nListNum

          nestedType [] = []
          nestedType (nesN:ns)
            | nesN >= 0 && nesN < 10 = toEnum nesN : nestedType ns
            | otherwise = TypeVoid : nestedType ns

          listType = nestedType listNum


typeToFigureInt :: [TypeTag] -> Number [Int]
typeToFigureInt [] = NumberNatural []
typeToFigureInt (TypeNegative:ts) = NumberNegative $ nestedFigure ts
typeToFigureInt tts = NumberNatural $ nestedFigure tts

nestedFigure :: [TypeTag] -> [Int]
nestedFigure [] = []
nestedFigure (t:ts)
    | curFigure < 10 = curFigure : nestedFigure ts
    | otherwise = nestedFigure ts

    where curFigure = fromEnum t


mathDirKey :: Direct -> SpecialKey -> Ordering
mathDirKey Horizontal KeyUp = LT
mathDirKey Horizontal KeyDown = GT
mathDirKey Horizontal _ = EQ
mathDirKey Vertical KeyLeft = LT
mathDirKey Vertical KeyRight = GT
mathDirKey Vertical _ = EQ


showEditSet :: EditSet -> String
showEditSet (EditStartLine (Right e)) = show e
showEditSet (EditLengthExpr (Right e)) = show e
showEditSet (EditQuantityQuestion (Right e)) = show e
showEditSet (EditTheme (Right e)) = show e
showEditSet (EditRangeRows _ _ (Right e)) = show e


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


replicateRightSet :: EditSet -> EditSet
replicateRightSet (EditStartLine {}) = EditStartLine $ Right initStartLine
replicateRightSet (EditLengthExpr {}) = EditLengthExpr $ Right initLengthExpr
replicateRightSet (EditQuantityQuestion {}) = EditQuantityQuestion $ Right initQuantityQuestion
replicateRightSet (EditTheme (Right right)) = EditTheme $ Right right
replicateRightSet (EditTheme (Left TypeVoid)) = EditTheme $ Right initTheme
replicateRightSet (EditTheme (Left (TypeInt int))) = EditTheme $ Right $ toEnum int
replicateRightSet (EditRangeRows {}) = EditRangeRows False Horizontal $ Right initRangeRows


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

typeToSet r
    | isRightInSet r = Just r
    | otherwise = Nothing


funcFromI :: Int -> [TypeTag]
funcFromI i = figureIntToType $ numInRows i

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


lazyPowerInAbacus :: StdGen -> (Int, Int) -> (Int, Int) -> ([RowAbacus], StdGen)
lazyPowerInAbacus gen tupleRange power = if abacusInNum pureLazy == 0
    then lazyPowerInAbacus finalGen tupleRange power
    else (pureLazy, finalGen)

    where (lenCurAbacus, firstGen) = randomR tupleRange gen
          
          pureLazy = fmap fst dirtLazy
          finalGen = snd $ last dirtLazy
          dirtLazy = take lenCurAbacus $ nestedLazy firstGen

          nestedLazy gen =
            let ready@(readyRow, newGen) = powerInAbacus gen power
            in ready : nestedLazy newGen


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


--numInAbacus :: Int -> [RowAbacus]


abacusInNum :: [RowAbacus] -> Int
abacusInNum abacus = internalNum $ zip (iterate (*10) 1) abacus

    where internalNum [] = 0
          internalNum ((row, RowAbacus lower upper):abacusis) = let numUpper = if upper then 5 * row else 0
            in length lower * row + numUpper + internalNum abacusis


exprAbacusInList :: [Abacus] -> [String]
exprAbacusInList [] = []
exprAbacusInList (Plus:as) = " + " : exprAbacusInList as
exprAbacusInList (Minus:as) = " - " : exprAbacusInList as
exprAbacusInList (Equal:as) = " = " : exprAbacusInList as
exprAbacusInList (Abacus abacus:as) = show (abacusInNum abacus) : exprAbacusInList as
