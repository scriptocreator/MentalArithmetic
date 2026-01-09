module Main (main) where

import Abacus
import TypeAbacus
import PureFunctions
import LazyFunctions
import ImperativeAbacus

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import ImperativeAbacus (Expressions(Expressions))



halfHoriz :: Int
halfHoriz = 1920 `div` 2

halfVert :: Int
halfVert = 1040 `div` 2

initWord :: StdGen -> World
initWord gen = EditSettings [EditStartLine (Right $ StartLine (-halfHoriz))] :\^/ EditApp [EditRandom gen]


picture :: World -> Picture
picture (EditSettings _ :\^/ _) = Blank
picture (App (Expressions exprs) _ _ :\^/ Settings (StartLine start) (LengthExpr lenExpr) _ _ _) = pictures allPicturies

    where funcSplitLinesAbacus :: [Abacus] -> [String]
          funcSplitLinesAbacus a = fmap unlines $ splitList lenExpr $ exprAbacusInList a

          splitLinesAbacus = fmap funcSplitLinesAbacus exprs

          (_, allPicturies) = foldl outputAbacusis (start, []) splitLinesAbacus

outputAbacusis :: (Int, [Picture]) -> [String] -> (Int, [Picture])
outputAbacusis st [] = st
outputAbacusis (current, predPictures) (str:ss)
    | current < (-halfVert) = (current + 40, predPictures)
    | current > halfVert = outputAbacusis (newCurrent, predPictures ++ [Text str]) ss
    | otherwise = outputAbacusis (newCurrent, predPictures) ss
    where newCurrent = current + 20


handleKey :: Event -> World -> World
handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (world@(EditApp {}) :\^/ set@(EditSettings {}))
    = error "В состоянии редактирования, невозможно переключиться на приложение"

handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) world@(EditSettings listMSet :\^/ EditApp listMApp)
    | isJust mlistSet && isJust mlistApp
        = App (Expressions finalExpr) finalGen carr :\^/ Settings start lenExpr quant theme range
    | otherwise = world
    
    where mlistSet = sortEditSet listMSet
          mlistApp = sortEditApp listMApp
          [EditStartLine (Right start), EditLengthExpr (Right lenExpr), EditQuantityQuestion (Right quant), EditTheme (Right theme), EditRangeRows (Right range)] =
            fmap typeToSet $ fromJust mlistSet
          [EditExpressions expr, EditRandom rand, EditCarriage carr] = fromJust mlistApp

          LengthExpr pureLenExpr = lenExpr
          QuantityQuestion pureQuant = quant

          (abac, rep) = case theme of
            Merely -> (curMerelyAbacus, repeatExpr) 
            Brother -> (curBrotherAbacus, repeatExprBro)
            Friend -> (curFriendAbacus, repeatExpr)

          adjustmentLenExpr = pureLenExpr `div` 2
          (finalExpr, finalGen) = generator rand pureQuant

          generator :: StdGen -> Int -> ([[Abacus]], StdGen)
          generator gen 0 = ([], gen)
          generator gen n = ((Abacus baseAbacus : freeBaseAbacus) : futureGenericExpr, finalGen)

                where (baseAbacus, firstGen) =
                        let (baseRow, nullGen) = powerInAbacus gen newAbacus
                        in (replicate pureLenExpr baseRow, nullGen)
                      (lazyReadyExpr, secondGen) = rep firstGen
                      readyExpr = take adjustmentLenExpr lazyReadyExpr
                      (freeBaseAbacus, thirdGen) = abac secondGen baseAbacus readyExpr
                      (futureGenericExpr, finalGen) = generator thirdGen $ pred n


handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (App expr rand carr :\^/ Settings start lenExpr quant theme range)
    = EditSettings editListSet :\^/ EditApp editListApp

    where editListApp = [EditExpressions expr, EditRandom rand, EditCarriage carr]
          editListSet = [EditStartLine (Right start), EditLengthExpr (Right lenExpr), EditQuantityQuestion (Right quant), EditTheme (Right theme), EditRangeRows (Right range)]

handleKey (EventKey (SpecialKey KeyUp) Down _ _) world@(EditSettings listMSet :\^/ app)
    | not (null listMSet) && (futureSet /= EditSetVoid) = if isJust mFindSet
        then EditSettings (findSet : freeListMSet) :\^/ app
        else EditSettings (futureSet : listMSet) :\^/ app
    | otherwise = world

    where futureSet = toEnum $ pred $ fromEnum $ head listMSet
          mFindSet = find (mathElemEditSet futureSet) listMSet
          findSet = fromJust mFindSet
          freeListMSet = delElem (mathElemEditSet futureSet) listMSet

handleKey (EventKey (SpecialKey KeyDown) Down _ _) world@(EditSettings listMSet :\^/ app)
    | not (null listMSet) && (futureSet /= EditSetVoid) = if isJust mFindSet
        then EditSettings (findSet : freeListMSet) :\^/ app
        else EditSettings (futureSet : listMSet) :\^/ app
    | otherwise = world

    where futureSet = toEnum $ succ $ fromEnum $ head listMSet
          mFindSet = find (mathElemEditSet futureSet) listMSet
          findSet = fromJust mFindSet
          freeListMSet = delElem (mathElemEditSet futureSet) listMSet

handleKey _ world = world


update _ world = world


main :: IO ()
main = do
    gen <- newStdGen
    play (InWindow "Random Abacus" (1920, 1040) (0, 0))
         white
         30
         (initWord gen)
         picture
         handleKey
         update
