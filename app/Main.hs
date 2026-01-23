module Main (main) where

import Abacus
import TypeAbacus
import TypeImperativeAbacus
import PureFunctions
import LazyFunctions
import ImperativeAbacus

import System.Random ( StdGen, newStdGen )
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Data.Maybe (fromJust, isJust, isNothing, catMaybes)
import Data.List (find)
import Data.Either (fromLeft)
import Data.Tuple.Extra



horiz = 1500 :: Int
vert = 800 :: Int

halfHoriz :: Int
halfHoriz = horiz `div` 2

halfVert :: Int
halfVert = vert `div` 2

initWorld :: StdGen -> World
initWorld gen
    = EditSettings [EditStartLine (Right $ StartLine (-halfHoriz))]
    :\^/ App (Expressions []) gen (Carriage 0) (50, 20, 10) (Carriage 0)


picture :: World -> Picture
picture (EditSettings sets :\^/ App _ _ _ indent mCarrSet) = pictures allPicturies

    where graphs = setToGraph sets
          zipSetGraph = zip sets graphs
          unitZip =
            let (headZ:tailZ) = zipSetGraph
                (setZ, graphZ) = headZ
            in (setZ, GraphMarker graphZ) : tailZ
          sortSetGraph = sortSetGraphList unitZip
          newGraphs = fmap snd sortSetGraph
          (allPicturies, _) = foldl
            (outputSettings Vertical funcVert False indent)
            ([], (0, 0)) -- (fromIntegral (-halfHoriz), fromIntegral halfVert))
            newGraphs

picture (App (Expressions exprs) _ _ _ _ :\^/ Settings (StartLine start) (LengthExpr lenExpr) _ _ _) = pictures allPicturies

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


outputSettings :: Direct
               -> ((Float, Float, Float) -> (Float, Float) -> (Float, Float))
               -> Bool
               -> (Float, Float, Float)
               -> ([Picture], (Float, Float))
               -> Graph String String
               -> ([Picture], (Float, Float))

outputSettings dir f log base stateFoldl (GraphMarker graph)
    = outputSettings dir f True base stateFoldl graph

outputSettings dir f log baseDirect (oldGraph, curDirect@(curHoriz, curVert)) (GraphElement name string)
    = let nameDirect@(nameHoriz, nameVert) = funcHoriz baseDirect curDirect
          futureDirect = f baseDirect nameDirect
          image = [Translate nameHoriz nameVert $ Scale 0.6 0.6 $ Color (selectColor log) $ Text name
                  ,Translate curHoriz curVert $ Scale 0.6 0.6 $ Color black $ Text string]
    
    in (oldGraph ++ [pictures image], futureDirect)

outputSettings dir f log baseDirect (oldGraph, curDirect) (GraphHorizontal name graphs) = (allGraph, posDirect)

    where nameDirect@(nameHoriz, nameVert) = funcHoriz baseDirect curDirect
          indentDirect = if isHoriz dir
            then (fst nameDirect, snd nameDirect - thd3 baseDirect)
            else nameDirect

          (futureGraph, futureDirect) = foldl (outputSettings Horizontal funcHoriz False baseDirect) ([], indentDirect) graphs
          curGraph = Translate nameHoriz nameVert $ Scale 0.6 0.6 $ Color (selectColor log) $ Text name
          allGraph = oldGraph ++ return curGraph ++ futureGraph
          dumpDirect = if isHoriz dir
            then (fst futureDirect, snd curDirect)
            else (fst curDirect, snd futureDirect)
          posDirect = f baseDirect dumpDirect

outputSettings dir f log baseDirect (oldGraph, curDirect) (GraphVertical name graphs) = (allGraph, posDirect)

    where nameDirect@(nameHoriz, nameVert) = funcHoriz baseDirect curDirect
          indentDirect = if isVert dir
            then (fst nameDirect + thd3 baseDirect, snd nameDirect)
            else nameDirect
          
          (futureGraph, futureDirect) = foldl (outputSettings Vertical funcVert False baseDirect) ([], indentDirect) graphs
          curGraph = Translate nameHoriz nameVert $ Scale 0.6 0.6 $ Color (selectColor log) $ Text name
          allGraph = oldGraph ++ futureGraph
          dumpDirect = if isVert dir
            then (fst curDirect, snd futureDirect)
            else (fst futureDirect, snd curDirect)
          posDirect = f baseDirect dumpDirect


funcHoriz :: (Float, Float, Float) -> (Float, Float) -> (Float, Float)
funcHoriz (baseHoriz, _, _) (horiz, vert) = (horiz + baseHoriz, vert)

funcVert :: (Float, Float, Float) -> (Float, Float) -> (Float, Float)
funcVert (_, baseVert, _) (horiz, vert) = (horiz, vert - baseVert)

selectColor :: Bool -> Color
selectColor False = blue
selectColor True = red


handleKey :: Event -> World -> World
handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (world@(App {}) :\^/ set@(EditSettings {}))
    = error "В состоянии редактирования, невозможно переключиться на редактирование приложения"


handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) world@(EditSettings listEffSet :\^/ app)
    | isJust mListSet && all isJust listMSet && theme /= ThemeVoid
        = App (Expressions finalExpr) finalGen mCarrApp indents mCarrSet :\^/ Settings start lenExpr quant theme range
    | otherwise = world

    where mListSet = sortEditSet listEffSet
          listMSet = fmap typeToSet $ fromJust mListSet
          listSet = catMaybes listMSet
          [EditStartLine (Right start), EditLengthExpr (Right lenExpr), EditQuantityQuestion (Right quant), EditTheme (Right theme), EditRangeRows _ _ (Right range)] = listSet
          App (Expressions expr) rand mCarrApp indents mCarrSet = app

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
                      (futureGenericExpr, finalGen) = (generator thirdGen . pred) n


handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (app :\^/ Settings start lenExpr quant theme range)
    = EditSettings editListSet :\^/ app

    where editListSet = [EditStartLine (Right start)
                        ,EditLengthExpr (Right lenExpr)
                        ,EditQuantityQuestion (Right quant)
                        ,EditTheme (Right theme)
                        ,EditRangeRows False Horizontal (Right range)]


handleKey (EventKey (SpecialKey specKey) Down _ _) world@(EditSettings _ :\^/ _)
    | specKey `elem` [KeyLeft, KeyRight, KeyUp, KeyDown] = multiDirect specKey world
    | otherwise = world


handleKey (EventKey (Char key) Down _ _) world@(EditSettings _ :\^/ _)
    | key `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-'] = inputData numType world
    | otherwise = world

    where numType = figureCharToType key


handleKey _ world = world


inputData :: TypeTag -> World -> World
inputData numType world@(EditSettings listEffSet :\^/ app)
    | isJust mNewHeadSet = EditSettings (newHeadSet : tail listEffSet) :\^/ app
    | otherwise = world

    where headSet = head listEffSet
          mNewHeadSet = nestedPutSet headSet numType
          newHeadSet = fromJust mNewHeadSet


nestedPutSet :: EditSet -> TypeTag -> Maybe EditSet
nestedPutSet headListEffSet putNum
    | fromEnumArgs headListEffSet == 1 && not (isTypeInt nTypeListOne) = return newOneArgSet
    | fromEnumArgs headListEffSet > 1 && nested headListEffSet = return newMultiArgSet
    | otherwise = Nothing

    where newOneArgSet = (funcPutEdit setWithLeft . newTypeList) nTypeListOne

          nTypeListOne = funcGetEdit setWithLeft

          newMultiArgSet = funcPutEdit setWithLeft $ TypeList (newTypeHeadEdit : tail listFromTypeEdit)

          newTypeHeadEdit = typeHeadEdit {right = newTypeList nTypeList}
          nTypeList = right typeHeadEdit
          typeHeadEdit = head listFromTypeEdit
          listFromTypeEdit = list $ funcGetEdit setWithLeft

          newTypeList n = case n of
            TypeList listFromType -> TypeList $ inliningNum putNum listFromType
            _ -> TypeList [putNum, TypeBool True]
          setWithLeft = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet


multiDirect :: SpecialKey -> World -> World
multiDirect key world@(EditSettings listEffSet :\^/ app)
    | fromEnumArgs headListEffSet == 1
        = funcWorld $
        funcIf (isTypeInt tTypeFromLeft)
               (case mathDirKey Vertical key of
                LT -> Nothing -- Ничего не делаю
                EQ -> return (funcPutEdit s (TypeInt futureInt) : tailListEffSet) -- Переключаю theme
                GT -> Nothing) $ -- Ничего не делаю
        funcIf (isNothing mMarker || not (bool marker))
               (case mathDirKey Vertical key of
                LT -> Nothing -- Ничего не делаю
                EQ -> funcIf (futureSet == EditSetVoid) Nothing $ -- Меняю элемент Set
                      funcIf (isJust mFindSet)
                             (return (findSet : freeListMSet))
                             (return (futureSet : listEffSet))
                GT -> return (funcPutEdit s (returnL $ inliningMarker listFromTypeLeft) : tailListEffSet)) -- Ставлю маркер
               (case mathDirKey Horizontal key of
                LT -> return (funcPutEdit s (returnL $ offMarker listFromTypeLeft) : tailListEffSet) -- Отключаю маркер
                EQ -> return (funcPutEdit s (returnL $ moveMarker listFromTypeLeft) : tailListEffSet) -- Перемещаю маркер
                GT -> Nothing) -- Ничего не делаю
    | fromEnumArgs headListEffSet > 1 && nested headListEffSet
        = funcWorld $ if isNothing mMarker || not (bool marker)
            then case mathDirKey dir key of
                LT -> return (headListEffSet {nested = False} : tailListEffSet) -- Отключить nested
                EQ -> if isJust mMoveType -- Переключить элемент Type
                    then return (funcPutEdit s moveType : tailListEffSet)
                    else Nothing
                GT -> return (funcPutEdit s (returnL $ onMarker_Multi listFromType_Multi) : tailListEffSet) -- Включить marker
            else case mathDirKey dir key of
                LT -> return (funcPutEdit s (returnL $ offMarker_Multi listFromType_Multi) : tailListEffSet) -- Отключить маркер
                EQ -> return (funcPutEdit s (returnL $ moveMarker_Multi listFromType_Multi) : tailListEffSet) -- Перемещаю маркер
                GT -> Nothing -- Ничего не делаю
    | futureSet /= EditSetVoid = if isJust mFindSet
        then EditSettings (findSet : freeListMSet) :\^/ app
        else EditSettings (futureSet : listEffSet) :\^/ app
    | otherwise = world

    where (headListEffSet:tailListEffSet) = listEffSet
          s = headListEffSet
          returnL = TypeList
          headSet = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet
          tTypeFromLeft = funcGetEdit headSet
          typeFromLeft init = case tTypeFromLeft of
            TypeList _ -> tTypeFromLeft
            _ -> init
          initList = TypeList []
          typeListFromLeft = typeFromLeft initList
          listFromTypeLeft = list typeListFromLeft
          funcDir = if key `elem` [KeyLeft, KeyUp] then pred else succ
          funcWorld Nothing = EditSettings listEffSet :\^/ app
          funcWorld (Just newListSet) = EditSettings newListSet :\^/ app

          futureInt :: Int
          futureInt = ((fromEnum :: Theme -> Int) . toEnum . funcDir . enumSet) headListEffSet

          offMarker = subst (TypeBool False) isTypeBool
          moveMarker = move (keyInBool key) isTypeBool

          mMarker = find isTypeBool listFromTypeLeft
          marker = fromJust mMarker

          mMoveType = nestedSwitchType typeListFromLeft enumHead funcDir
          moveType = fromJust mMoveType
          onMarker_Multi l = curPair {right = TypeList $ inliningMarker l} : freeHeadList
          offMarker_Multi l = curPair {right = TypeList $ subst (TypeBool False) isTypeBool l} : freeHeadList
          moveMarker_Multi l = curPair {right = TypeList $ move (keyInBool key) isTypeBool l} : freeHeadList

          enumHead = (enumArgsSet . initRightSet) headListEffSet
          dir = direct headListEffSet
          (curPair:freeHeadList) = listFromTypeLeft
          tListFromType_Multi = right curPair
          listFromType_Multi = list tListFromType_Multi

          futureSet = (toEnum . funcDir . fromEnum) headListEffSet
          mFindSet = find (mathElemEditSet futureSet) listEffSet
          findSet = fromJust mFindSet

          freeListMSet = delElem (mathElemEditSet futureSet) listEffSet


nestedSwitchType :: TypeTag -> Int -> (Int -> Int) -> Maybe TypeTag
nestedSwitchType lTypeTags enumSet dir
    | futureNested >= 0 && futureNested <= enumSet
        = if isJust mFindType
            then return newListFindTypes
            else return newListCreateTypes
    | otherwise = Nothing

    where lFromTypeTags = list lTypeTags
          (curPair:freeHeadList) = lFromTypeTags
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
