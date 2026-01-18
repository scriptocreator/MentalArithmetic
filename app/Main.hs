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
import Data.Maybe (fromJust, isJust, isNothing, catMaybes)
import Data.List (find)
import Data.Either (fromLeft)
import PureFunctions (lengthType)



horiz = 1920 :: Int
vert = 1040 :: Int

halfHoriz :: Int
halfHoriz = horiz `div` 2

halfVert :: Int
halfVert = vert `div` 2

initWorld :: StdGen -> World
initWorld gen = EditSettings [EditStartLine (Right $ StartLine (-halfHoriz))] :\^/ EditApp [EditRandom gen]


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


handleKey (EventKey (SpecialKey specKey) Down _ _) world@(EditSettings _ :\^/ _)
    | specKey `elem` [KeyLeft, KeyRight, KeyUp, KeyDown] = multiDirect specKey world
    | otherwise = world


handleKey (EventKey (Char key) Down _ _) world@(EditSettings _ :\^/ _)
    | key `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] = inputData numType world
    | otherwise = world

    where numType = figureCharToType key


handleKey _ world = world


inputData numType world@(EditSettings listEffSet :\^/ app)
    | isJust mNewHeadSet = EditSettings (newHeadSet : tail listEffSet) :\^/ app
    | otherwise = world

    where headSet = head listEffSet
          mNewHeadSet = nestedPutSet headSet numType
          newHeadSet = fromJust mNewHeadSet


nestedPutSet :: EditSet -> TypeTag -> Maybe EditSet
nestedPutSet headListEffSet putNum
    | fromEnumArgs headListEffSet == 1 = return newOneArgSet
    | fromEnumArgs headListEffSet > 1 && nested headListEffSet = return newMultiArgSet
    | otherwise = Nothing

    where newOneArgSet = funcEdit setWithLeft newTypeListOne

          newTypeListOne = case nTypeListOne of
            TypeList listFromType -> TypeList $ inliningNum putNum listFromType
            _ -> TypeList [putNum, TypeBool True]

          nTypeListOne = funcGetEdit setWithLeftOne
          setWithLeftOne = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet

          newMultiArgSet = funcEdit setWithLeft $ TypeList (newTypeHeadEdit : tail listFromTypeEdit)
          newTypeHeadEdit = typeHeadEdit {right = newTypeList}

          newTypeList = case nTypeList of
            TypeList listFromType -> TypeList $ inliningNum putNum listFromType
            _ -> TypeList [putNum, TypeBool True]

          nTypeList = right typeHeadEdit
          typeHeadEdit = head listFromTypeEdit
          listFromTypeEdit = list $ funcGetEdit setWithLeft
          setWithLeft = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet

-- Исправить подстановку голого `futureSet`
multiDirect :: SpecialKey -> World -> World
multiDirect key world@(EditSettings listEffSet :\^/ app)
    | fromEnumArgs headListEffSet == 1
        = funcWorld $ if isNothing mMarker || not (bool marker)
            then case mathDirKey Vertical key of
                LT -> Nothing -- Ничего не делаю
                EQ -> funcIf (futureSet == EditSetVoid) Nothing $ -- Меняю элемент Set
                      funcIf (isJust mFindSet)
                             (return (findSet : freeListMSet))
                             (return (futureSet : listEffSet))
                GT -> return (funcEdit s (returnE $ inliningMarker listFromType) : tailListEffSet) -- Ставлю маркер
            else case mathDirKey Vertical key of
                LT -> return (funcEdit s (returnE $ offMarker listFromType) : tailListEffSet) -- Отключаю маркер
                EQ -> return (funcEdit s (returnE $ moveMarker listFromType) : tailListEffSet) -- Перемещаю маркер
                GT -> Nothing -- Ничего не делаю
    | fromEnumArgs headListEffSet > 1 && nested headListEffSet
        = funcWorld $ if isNothing mMarker || not (bool marker)
            then case mathDirKey dir key of
                LT -> return (headListEffSet {nested = False} : tailListEffSet) -- Отключить nested
                EQ -> if isJust mMoveType -- Переключить элемент Type
                    then return (funcEdit s moveType : tailListEffSet)
                    else Nothing
                GT -> return (funcEdit s (returnE $ onMarker_Multi listFromType_Multi) : tailListEffSet) -- Включить marker
            else case mathDirKey dir key of
                LT -> return (funcEdit s (returnE $ offMarker_Multi listFromType_Multi) : tailListEffSet) -- Отключить маркер
                EQ -> return (funcEdit s (returnE $ moveMarker_Multi listFromType_Multi) : tailListEffSet) -- Перемещаю маркер
                GT -> Nothing -- Ничего не делаю
    | futureSet /= EditSetVoid = if isJust mFindSet
        then EditSettings (findSet : freeListMSet) :\^/ app
        else EditSettings (futureSet : listEffSet) :\^/ app
    | otherwise = world

    where (headListEffSet:tailListEffSet) = listEffSet
          s = headListEffSet
          returnE = TypeList
          newHead = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet
          tTypeFromLeft = funcGetEdit newHead
          listFromType = list tTypeFromLeft
          funcWorld Nothing = EditSettings listEffSet :\^/ app
          funcWorld (Just newListSet) = EditSettings newListSet :\^/ app

          offMarker = subst (TypeBool False) isTypeBool
          moveMarker = move (keyInBool key) isTypeBool

          mMarker = find isTypeBool listFromType
          marker = fromJust mMarker

          mMoveType = nestedSwitchType tTypeFromLeft enumHead funcDir
          moveType = fromJust mMoveType
          onMarker_Multi l = curPair {right = TypeList $ inliningMarker l} : freeHeadList
          offMarker_Multi l = curPair {right = TypeList $ subst (TypeBool False) isTypeBool l} : freeHeadList
          moveMarker_Multi l = curPair {right = TypeList $ move (keyInBool key) isTypeBool l} : freeHeadList

          enumHead = enumArgsSet $ initRightSet headListEffSet
          dir = direct headListEffSet
          funcDir = if key `elem` [KeyLeft, KeyUp] then pred else succ
          (curPair:freeHeadList) = listFromType
          tListFromType_Multi = right curPair
          listFromType_Multi = list tListFromType_Multi

          futureSet = toEnum $ pred $ fromEnum headListEffSet
          mFindSet = find (mathElemEditSet futureSet) listEffSet
          findSet = fromJust mFindSet

          freeListMSet = delElem (mathElemEditSet futureSet) listEffSet


--nestedSwitchSet :: [EditSet] -> SpecialKey -> (Int -> Int) -> Third EditSet EditSet
--nestedSwitchSet (headListEffSet:tailListEffSet) key funcDir


nestedSwitchType :: TypeTag -> Int -> (Int -> Int) -> Maybe TypeTag
nestedSwitchType lTypeTags enumSet dir
    | futureNested >= 0 && futureNested <= enumSet
        = if isJust mFindType
            then return newListFindTypes
            else return newListCreateTypes
    | otherwise = Nothing

    where lFromTypeTags = list lTypeTags
          (curPair:freeHeadList) = list lTypeTags
          TypeInt nestedOfType = left curPair
          futureNested = dir nestedOfType
          mFindType = findType mathType lTypeTags
          pureFindType = fromJust mFindType

          freeFindList = delElem mathType lFromTypeTags
          newListFindTypes = TypeList (pureFindType : freeFindList)

          newPair = TypePair (TypeInt futureNested) TypeVoid
          newListCreateTypes = TypeList (newPair : freeHeadList)

          mathType a | left a == TypeInt futureNested = True
          mathType _ = False


update _ world = world


main :: IO ()
main = do
    gen <- newStdGen
    play (InWindow "Random Abacus" (horiz, vert) (0, 0))
         white
         30
         (initWorld gen)
         picture
         handleKey
         update
