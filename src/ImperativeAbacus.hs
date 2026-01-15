{-# LANGUAGE Strict #-}

module ImperativeAbacus where

import TypeAbacus
import TypeImperativeAbacus
import PureFunctions ( sortTypeList, prodTypeToList, sumTypeToList, tailType )

import System.Random
import System.Random.Stateful
import Data.Maybe (fromJust, isJust)
import Graphics.Gloss.Interface.Pure.Game



mathDirKey :: Direct -> SpecialKey -> Bool
mathDirKey Horizontal KeyLeft = True
mathDirKey Horizontal KeyRight = True
mathDirKey Vertical KeyUp = True
mathDirKey Vertical KeyDown = True
mathDirKey _ _ = False


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
setToType (EditTheme (Right Merely)) = EditTheme $ Left $ TypeInt 0
setToType (EditTheme (Right Brother)) = EditTheme $ Left $ TypeInt 1
setToType (EditTheme (Right Friend)) = EditTheme $ Left $ TypeInt 2
setToType (EditRangeRows nes dir (Right (RangeRows firstI secondI)))
    = EditRangeRows nes dir $ Left $
    TypeList [TypeComp (TypeInt 0 :#+ TypeInt firstI), TypeComp (TypeInt 1 :#+ TypeInt secondI)]
setToType typeTag = typeTag


typeToSet :: EditSet -> Maybe EditSet
typeToSet (EditStartLine (Left (TypeInt start))) = Just $ EditStartLine $ Right $ StartLine start
typeToSet (EditLengthExpr (Left (TypeInt lenExpr))) = Just $ EditLengthExpr $ Right $ LengthExpr lenExpr
typeToSet (EditQuantityQuestion (Left (TypeInt quant))) = Just $ EditQuantityQuestion $ Right $ QuantityQuestion quant
typeToSet (EditTheme (Left (TypeInt 0))) = Just $ EditTheme $ Right $ Merely
typeToSet (EditTheme (Left (TypeInt 1))) = Just $ EditTheme $ Right $ Brother
typeToSet (EditTheme (Left (TypeInt 2))) = Just $ EditTheme $ Right $ Friend
typeToSet (EditRangeRows nes dir (Left (TypeList myType@[TypeComp (TypeInt _ :#+ TypeInt _), TypeComp (TypeInt _ :#+ TypeInt _)])))
    = if length typeList == fromEnumArgs initRangeRows
        then Just $ EditRangeRows nes dir $ Right $ RangeRows firstI secondI
        else Nothing

    where typeList = fmap tailType $ sortTypeList myType
          [TypeInt firstI, TypeInt secondI] = typeList

typeToSet right | isRightInSet right = Just right
typeToSet _ = Nothing


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
          nestedSort expr rand carr (x@(EditRandom {}):xs) = nestedSort expr (return x) carr xs
          nestedSort expr rand carr (x@(EditCarriage {}):xs) = nestedSort expr rand (return x) xs
          nestedSort _ _ _ _ = Nothing


sortEditSet :: [EditSet] -> Maybe [EditSet]
sortEditSet = nestedSort Nothing Nothing Nothing Nothing Nothing

    where nestedSort (Just start) (Just lenExpr) (Just quant) (Just theme) (Just range) [] = Just [start, lenExpr, quant, theme, range, range]
          nestedSort start lenExpr quant theme range (x@(EditStartLine {}):xs) = nestedSort (return x) lenExpr quant theme range xs
          nestedSort start lenExpr quant theme range (x@(EditLengthExpr {}):xs) = nestedSort start (return x) quant theme range xs
          nestedSort start lenExpr quant theme range (x@(EditQuantityQuestion {}):xs) = nestedSort start lenExpr (return x) theme range xs
          nestedSort start lenExpr quant theme range (x@(EditTheme {}):xs) = nestedSort start lenExpr quant (return x) range xs
          nestedSort start lenExpr quant theme range (x@(EditRangeRows {}):xs) = nestedSort start lenExpr quant theme (return x) xs
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
