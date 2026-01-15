module Main (main) where

import Abacus
import TypeAbacus
import TypeImperativeAbacus
import PureFunctions
import LazyFunctions
import ImperativeAbacus

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (find)



horiz = 1920 :: Int
vert = 1040 :: Int

halfHoriz :: Int
halfHoriz = horiz `div` 2

halfVert :: Int
halfVert = vert `div` 2

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
    = error "В состоянии редактирования, невозможно переключиться на редактирование приложения"


handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) world@(EditSettings listEffSet :\^/ EditApp listEffApp)
    | isJust mListSet && isJust mListApp && all isJust listMSet
        = App (Expressions finalExpr) finalGen carr :\^/ Settings start lenExpr quant theme range
    | otherwise = world

    where mListSet = sortEditSet listEffSet
          mListApp = sortEditApp listEffApp
          listMSet = fmap typeToSet $ fromJust mListSet
          listSet = catMaybes listMSet
          [EditStartLine (Right start), EditLengthExpr (Right lenExpr), EditQuantityQuestion (Right quant), EditTheme (Right theme), EditRangeRows _ _ (Right range)] = listSet
          [EditExpressions expr, EditRandom rand, EditCarriage carr] = fromJust mListApp

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
          editListSet = [EditStartLine (Right start)
                        ,EditLengthExpr (Right lenExpr)
                        ,EditQuantityQuestion (Right quant)
                        ,EditTheme (Right theme)
                        ,EditRangeRows False Horizontal (Right range)]


handleKey (EventKey (SpecialKey KeyLeft) Down _ _) world@(EditSettings listEffSet :\^/ app) = multiDirect KeyLeft world
handleKey (EventKey (SpecialKey KeyRight) Down _ _) world@(EditSettings listEffSet :\^/ app) = multiDirect KeyRight world
handleKey (EventKey (SpecialKey KeyUp) Down _ _) world@(EditSettings listEffSet :\^/ app) = multiDirect KeyUp world
handleKey (EventKey (SpecialKey KeyDown) Down _ _) world@(EditSettings listEffSet :\^/ app) = multiDirect KeyDown world


handleKey _ world = world


multiDirect :: SpecialKey -> World -> World
multiDirect key world@(EditSettings listEffSet :\^/ app)
    | isThirdJust thNestedSet
        = EditSettings (pureNestedSet : tailListEffSet) :\^/ app
    | (futureSet /= EditSetVoid) = if isJust mFindSet
        then EditSettings (findSet : freeListMSet) :\^/ app
        else EditSettings (futureSet : listEffSet) :\^/ app
    | otherwise = world

    where funcDir = if key `elem` [KeyLeft, KeyUp] then pred else succ
          (headListEffSet:tailListEffSet) = listEffSet
          thNestedSet = nestedSet headListEffSet key funcDir
          pureNestedSet = if isThirdRight thNestedSet
            then fromThirdRight thNestedSet
            else fromThirdLeft thNestedSet

          futureSet = toEnum $ pred $ fromEnum headListEffSet
          mFindSet = find (mathElemEditSet futureSet) listEffSet
          findSet = fromJust mFindSet
          freeListMSet = delElem (mathElemEditSet futureSet) listEffSet


nestedSet :: EditSet -> SpecialKey -> (Int -> Int) -> Third EditSet EditSet
nestedSet headListEffSet key funcDir
    | not $ mathDirKey dir key = ThirdLeft $ headListEffSet {nested = False}
    | fromEnumArgs headListEffSet > 1 && nested headListEffSet && isJust mNewType
        = ThirdRight $ newHead {edit = Left newType}
    | otherwise = ThirdNothing

    where dir = direct headListEffSet
          newHead = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet
          enumHead = enumArgsSet $ initRightSet headListEffSet
          mNewType = nestedType (fromLeft $ edit newHead) enumHead funcDir
          newType = fromJust mNewType


nestedType :: TypeTag -> Int -> (Int -> Int) -> Maybe TypeTag
nestedType lTypeTags enumSet dir
    | futureNested >= 0 && futureNested <= enumSet
        = if isJust mFindType
            then return newListFindTypes
            else return newListCreateTypes
    | otherwise = Nothing

    where TypeInt nestedOfType = headType $ headType lTypeTags
          futureNested = dir nestedOfType
          mFindType = findType mathType lTypeTags
          pureFindType = fromJust mFindType
          freeListTypes = delElemType mathType lTypeTags
          newListFindTypes = pureFindType +#+ freeListTypes

          newType = TypeComp $ TypeInt futureNested :#+ TypeVoid
          newListCreateTypes = newType +#+ lTypeTags

          mathType a | headType a == (TypeInt futureNested) = True
          mathType _ = False


update _ world = world


main :: IO ()
main = do
    gen <- newStdGen
    play (InWindow "Random Abacus" (horiz, vert) (0, 0))
         white
         30
         (initWord gen)
         picture
         handleKey
         update
