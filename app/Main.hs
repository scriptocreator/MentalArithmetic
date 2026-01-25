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
import Data.Tuple.Extra ( fst3, snd3, thd3 )



horiz = 1500 :: Int
vert = 800 :: Int

halfHoriz :: Int
halfHoriz = horiz `div` 2

halfVert :: Int
halfVert = vert `div` 2

multi :: Float
multi = 0.3

initWorld :: StdGen -> World
initWorld gen
    = EditSettings [EditStartLine (Right $ StartLine (-halfHoriz))]
    :\^/ App (Expressions []) gen (Carriage 0) (300, 40, 10) (Carriage 0)


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
            ([], {-(0, 0))-} (fromIntegral (-halfHoriz), fromIntegral halfVert - snd3 indent))
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
    = let tailDirect@(tailHoriz, tailVert) = funcHoriz baseDirect curDirect
          futureDirect = f baseDirect tailDirect
          dumpDirect = if isHoriz dir
            then (fst futureDirect, snd curDirect)
            else (fst curDirect, snd futureDirect)
          image = [Translate curHoriz curVert $ Scale multi multi $ Color (selectColor log) $ Text name
                  ,Translate tailHoriz tailVert $ Scale multi multi $ Color black $ Text string]
    
    in (oldGraph ++ [pictures image], dumpDirect)

outputSettings dir f log baseDirect (oldGraph, curDirect@(curHoriz, curVert)) (GraphHorizontal name graphs)
    = (allGraph, posDirect)

    where tailDirect@(tailHoriz, tailVert) = funcHoriz baseDirect curDirect
          indentDirect = if isHoriz dir
            then (tailHoriz, tailVert - thd3 baseDirect)
            else tailDirect

          (futureGraph, (futureHoriz, futureVert)) = foldl
            (outputSettings Horizontal funcHoriz False baseDirect)
            ([], indentDirect)
            graphs

          curGraph = Translate curHoriz curVert $ Scale multi multi $ Color (selectColor log) $ Text name
          allGraph = oldGraph ++ return curGraph ++ futureGraph
          dumpDirect = if isHoriz dir
            then (futureHoriz, curVert)
            else (curHoriz, futureVert)
          posDirect = f baseDirect dumpDirect

outputSettings dir f log baseDirect (oldGraph, curDirect@(curHoriz, curVert)) (GraphVertical name graphs)
    = (allGraph, posDirect)

    where tailDirect@(tailHoriz, tailVert) = funcHoriz baseDirect curDirect
          indentDirect = if isVert dir
            then (tailHoriz + thd3 baseDirect, tailVert)
            else tailDirect
          
          (futureGraph, (futureHoriz, futureVert)) = foldl
            (outputSettings Vertical funcVert False baseDirect)
            ([], indentDirect)
            graphs

          curGraph = Translate curHoriz curVert $ Scale multi multi $ Color (selectColor log) $ Text name
          allGraph = oldGraph ++ return curGraph ++ futureGraph
          dumpDirect = if isVert dir
            then (curHoriz, futureVert)
            else (futureHoriz, curVert)
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
                      clearBaseAbacus = clearVoidRows baseAbacus
                      lazyReady = rep firstGen
                      readyExprGen = take adjustmentLenExpr lazyReady
                      secondGen = snd $ last readyExprGen
                      readyExpr = fmap fst readyExprGen
                      (freeBaseAbacus, thirdGen) = abac secondGen clearBaseAbacus readyExpr
                      (futureGenericExpr, finalGen) = (generator thirdGen . pred) n


handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (app :\^/ Settings start lenExpr quant theme range)
    = EditSettings editListSet :\^/ app

    where editListSet = [EditStartLine (Right start)
                        ,EditLengthExpr (Right lenExpr)
                        ,EditQuantityQuestion (Right quant)
                        ,EditTheme (Right theme)
                        ,EditRangeRows False Horizontal (Right range)]

-- `KeyBackspace` почему-то не работет
handleKey (EventKey (SpecialKey KeyDelete) Down _ _) world@(EditSettings sets :\^/ app)
    -- | True = WorldVoid
    | isJust mNewSet = EditSettings newSets :\^/ app
    | otherwise = world
    
    where (headSet:tailSets) = sets
          mNewSet = deleteData False headSet
          newSet = fromJust mNewSet
          newSets = newSet : tailSets

-- Вместо `KeyDelete` использую `KeyInsert`, для удобства
handleKey (EventKey (SpecialKey KeyInsert) Down _ _) world@(EditSettings sets :\^/ app)
    | isJust mNewSet = EditSettings newSets :\^/ app
    | otherwise = world
    
    where (headSet:tailSets) = sets
          mNewSet = deleteData True headSet
          newSet = fromJust mNewSet
          newSets = newSet : tailSets


handleKey (EventKey (SpecialKey specKey) Down _ _) world@(EditSettings _ :\^/ _)
    | specKey `elem` [KeyLeft, KeyRight, KeyUp, KeyDown] = multiDirect specKey world
    | otherwise = world


handleKey (EventKey (Char key) Down _ _) world@(EditSettings _ :\^/ _)
    | key `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-'] = inputData numType world
    | otherwise = world

    where numType = figureCharToType key


handleKey _ world = world


deleteData :: Bool -> EditSet -> Maybe EditSet
deleteData log headSet
    | nEdit == TypeVoid = Nothing
    | fromEnumArgs headSet == 1 && not (isTypeInt nEdit) = return newDelOneTypeNum
    | fromEnumArgs headSet > 1 && nested headSet = return newDelMultiArgSet
    | otherwise = Nothing

    where setWithLeft = if isRightInSet headSet
            then setToType headSet
            else headSet
          nEdit = funcGetEdit setWithLeft
          newTypeList n = case n of
            TypeList listFromType -> TypeList $ cut log isTypeBool listFromType
            _ -> TypeList [TypeBool True]

          newDelOneTypeNum = funcPutEdit setWithLeft (newTypeList nEdit)
          
          newDelMultiArgSet = funcPutEdit setWithLeft $ TypeList (newTypeHeadEdit : typeTailEdit)
          newTypeHeadEdit = typeHeadEdit {right = newTypeList $ right typeHeadEdit}

          (typeHeadEdit:typeTailEdit) = listFromTypeEdit
          listFromTypeEdit = list nEdit


inputData :: TypeTag -> World -> World
inputData numType world@(EditSettings listEffSet :\^/ app)
    | isJust mNewHeadSet = EditSettings (newHeadSet : tail listEffSet) :\^/ app
    | otherwise = world

    where headSet = head listEffSet
          mNewHeadSet = nestedPutSet headSet numType
          newHeadSet = fromJust mNewHeadSet


nestedPutSet :: EditSet -> TypeTag -> Maybe EditSet
nestedPutSet headListEffSet putNum
    | fromEnumArgs headListEffSet == 1 && not (isTypeInt nEdit) = return newOneArgSet
    | fromEnumArgs headListEffSet > 1 && nested headListEffSet =
        if nEdit == TypeVoid
            then return newCreateMultiArgSet
            else return newMultiArgSet
    | otherwise = Nothing

    where nEdit = funcGetEdit setWithLeft
          newTypeList n = case n of
            TypeList listFromType -> TypeList $ inliningNum putNum listFromType
            _ -> TypeList [putNum, TypeBool True]
          setWithLeft = if isRightInSet headListEffSet
            then setToType headListEffSet
            else headListEffSet

          newOneArgSet = (funcPutEdit setWithLeft . newTypeList) nEdit

          newCreateMultiArgSet = funcPutEdit setWithLeft $ TypeList [createPair]
          createPair = TypePair (TypeInt 1)  createType
          createType = TypeList [putNum, TypeBool True]

          newMultiArgSet = funcPutEdit setWithLeft $ TypeList (newTypeHeadEdit : typeTailEdit)
          newTypeHeadEdit = typeHeadEdit {right = newTypeList $ right typeHeadEdit}

          (typeHeadEdit:typeTailEdit) = listFromTypeEdit
          listFromTypeEdit = list nEdit


multiDirect :: SpecialKey -> World -> World
multiDirect key world@(EditSettings listEffSet :\^/ app)

    | fromEnumArgs headListEffSet == 1
        = funcWorld $
        funcIf  (enumArgsSet (replicateRightSet headListEffSet) == 0)
                (case mathDirKey Horizontal key of
                    LT -> swithSet -- Меняю элемент Set
                    EQ -> return (funcPutEdit s (TypeInt futureInt) : tailListEffSet) -- Переключаю theme
                    GT -> swithSet) $ -- Меняю элемент Set

        funcIf  (isNothing mOneMarker || not (bool oneMarker))
                (case mathDirKey Vertical key of
                    LT -> Nothing -- Ничего не делаю
                    EQ -> swithSet -- Меняю элемент Set
                    GT -> return (funcPutEdit s (returnL $ inliningMarker listFromTypeEdit) : tailListEffSet)) -- Ставлю маркер
                    
                (case mathDirKey Horizontal key of
                    LT -> return (funcPutEdit s (returnL $ offMarker listFromTypeEdit) : tailListEffSet) -- Отключаю маркер
                    EQ -> return (funcPutEdit s (returnL $ moveMarker listFromTypeEdit) : tailListEffSet) -- Перемещаю маркер
                    GT -> Nothing) -- Ничего не делаю

    | fromEnumArgs headListEffSet > 1
        = funcWorld $
        funcIf  (not (nested headListEffSet))
                (case mathDirKey Vertical key of
                    LT -> Nothing -- Ничего не делаю
                    EQ -> swithSet -- Меняю элемент Set
                    GT -> return (headListEffSet {nested = True} : tailListEffSet)) $ -- Включить nested

        funcIf  (isNothing mMultiMarker || not (bool multiMarker))
                (case mathDirKey dir key of
                    LT -> return (headListEffSet {nested = False} : tailListEffSet) -- Отключить nested
                    EQ -> if isJust mMoveType -- Переключить элемент Type
                        then return (funcPutEdit s moveType : tailListEffSet)
                        else Nothing
                    GT -> return (funcPutEdit s (returnL $ onMarker_Multi listFromTypeNumList_Multi) : tailListEffSet)) -- Включить marker

                (case mathDirKey dir key of
                    LT -> return (funcPutEdit s (returnL $ offMarker_Multi listFromTypeNumList_Multi) : tailListEffSet) -- Отключить маркер
                    EQ -> return (funcPutEdit s (returnL $ moveMarker_Multi listFromTypeNumList_Multi) : tailListEffSet) -- Перемещаю маркер
                    GT -> Nothing) -- Ничего не делаю

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
          tTypeFromEdit = funcGetEdit headSet
          typeFromEdit init = case tTypeFromEdit of
            TypeList _ -> tTypeFromEdit
            TypeInt _ -> tTypeFromEdit
            _ -> init
          initNumList = []
          initPair = TypePair (TypeInt 1) TypeVoid
          initList = TypeList [initPair]
          initInt = TypeInt 0
          typeIntFromEdit = typeFromEdit initInt
          typeListFromEdit = typeFromEdit initList
          listFromTypeEdit = list typeListFromEdit
          funcDir = if key `elem` [KeyLeft, KeyUp] then pred else succ
          funcWorld Nothing = EditSettings listEffSet :\^/ app
          funcWorld (Just newListSet) = EditSettings newListSet :\^/ app
          swithSet =
            funcIf  (futureSet == EditSetVoid) Nothing $
            funcIf  (isJust mFindSet)
                    (return (findSet : freeListMSet))
                    (return (futureSet : listEffSet))

          interimTheme = headListEffSet {editTheme = Left typeIntFromEdit}
          futureInt :: Int
          futureInt = ((fromEnum :: Theme -> Int) . toEnum . funcDir . enumSet . fromJust . typeToSet) interimTheme

          offMarker = subst (TypeBool False) isTypeBool
          moveMarker = move (keyInBool key) isTypeBool

          mOneMarker = find isTypeBool listFromTypeEdit
          oneMarker = fromJust mOneMarker

          mMoveType = nestedSwitchType key funcDir enumLimitSet typeListFromEdit
          moveType = fromJust mMoveType
          onMarker_Multi l = curPair {right = TypeList $ inliningMarker l} : freeHeadList
          offMarker_Multi l = curPair {right = TypeList $ subst (TypeBool False) isTypeBool l} : freeHeadList
          moveMarker_Multi l = curPair {right = TypeList $ move (keyInBool key) isTypeBool l} : freeHeadList

          mMultiMarker = find isTypeBool listFromTypeNumList_Multi
          multiMarker = fromJust mMultiMarker

          enumLimitSet = (enumArgsSet . replicateRightSet) headListEffSet
          dir = direct headListEffSet
          (curPair:freeHeadList) = listFromTypeEdit
          tListFromTypeNumList_Multi = right curPair
          listFromTypeNumList_Multi = case tListFromTypeNumList_Multi of
            TypeList l -> l
            _ -> initNumList

          futureSet = (toEnum . funcDir . fromEnum) headListEffSet
          mFindSet = find (mathElemEditSet futureSet) listEffSet
          findSet = fromJust mFindSet

          freeListMSet = delElem (mathElemEditSet futureSet) listEffSet


nestedSwitchType :: SpecialKey -> (Int -> Int) -> Int -> TypeTag -> Maybe TypeTag
nestedSwitchType _ _ _ TypeVoid = return $ TypeList [TypePair (TypeInt 1) TypeVoid]
nestedSwitchType key dirKey enumLimitSet lTypeTags
    | futureNested > 0 && futureNested <= enumLimitSet
        = if isJust mFindType
            then return newListFindTypes
            else return newListCreateTypes
    | otherwise = Nothing

    where newListFindTypes = TypeList (pureFindType : fmapList offMarker (head freeFindList) : tail freeFindList)
          freeFindList = delElem mathType lFromTypeTags
          
          newListCreateTypes = TypeList (newPair : fmapList offMarker (head lFromTypeTags) : tail lFromTypeTags)
          newPair = TypePair (TypeInt futureNested) TypeVoid

          fmapList f (TypeList l) = TypeList $ f l
          fmapList _ t = t
          offMarker = subst (TypeBool False) isTypeBool
          lFromTypeTags = list lTypeTags
          (curPair:freeHeadList) = lFromTypeTags
          futureNested = dirKey $ int $ left curPair
          mFindType = findType mathType lTypeTags
          pureFindType = fromJust mFindType

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
