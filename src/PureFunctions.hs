-- {-# LANGUAGE Strict #-}

module PureFunctions where

import TypeAbacus
import Text.Printf (printf)
import Data.List (find)
import Data.Maybe



funcIf :: Bool -> n -> n -> n
funcIf log a b = if log then a else b


delElem :: (a -> Bool) -> [a] -> [a]
delElem _ [] = []
delElem f (x:xs) | f x = xs
delElem f (x:xs) = x : delElem f xs


splitList :: Int -> [a] -> [[a]]
splitList divStand = nestedSplit 1 []

  where nestedSplit :: Int -> [a] -> [a] -> [[a]]
        nestedSplit _ interim [] = [interim]
        nestedSplit st interim (x:xs) | st == divStand =
          let predInterim = interim++return x
          in predInterim : nestedSplit (succ st) [] xs
        nestedSplit st interim (x:xs) =
          nestedSplit (succ st) (interim++return x) xs


sortTypeList :: [TypeTag] -> [TypeTag]
sortTypeList = nestedSort []

    where nestedSort :: [TypeTag] -> [TypeTag] -> [TypeTag]
          nestedSort sortList [] = sortList
          nestedSort sortList (t:ls) = nestedSort (putTypeList t sortList) ls

putTypeList :: TypeTag -> [TypeTag] -> [TypeTag]
putTypeList t [] = [t]
putTypeList t (tel:ls)
    | left t == left tel
        = error $ printf "Error PureFunctions №1: Обнаружена ещё одна переменная под аргумент конструктора – «%s» и «%s»"
                         (show arg1)
                         (show arg2)
    | left t < left tel = t : tel : ls
    | otherwise = tel : putTypeList t ls

    where arg1 = right t
          arg2 = right tel 


prodTypeToList :: TypeTag -> [[TypeTag]]
prodTypeToList (TypeComp a :#* ts) = (sumTypeToList a) : prodTypeToList ts
prodTypeToList (TypeComp a) = [TypeComp a] : []

sumTypeToList :: TypeTag -> [TypeTag]
sumTypeToList (a :#+ nts) = a : sumTypeToList nts
sumTypeToList a = [a]


(+#+) :: TypeTag -> TypeTag -> TypeTag
el +#+ (TypeNot n) = TypeNot $ el +#+! n
el +#+ (TypeComp c) = TypeComp $ el +#+! c
el +#+ (TypeList l) = TypeList (el:l)
el +#+ p@(_ :#* _) = el :#* p
el +#+ s@(_ :#+ _) = el :#+ s
el +#+ t = el :#* t

(+#+!) :: TypeTag -> TypeTag -> TypeTag
el +#+! n@(TypeNot _) = el :#* n
el +#+! c@(TypeComp _) = el :#* c
el +#+! (TypeList l) = TypeList (el:l)
el +#+! p@(_ :#* _) = el :#* p
el +#+! s@(_ :#+ _) = el :#+ s
el +#+! t = el :#* t


insertHeadType :: TypeTag -> TypeTag -> TypeTag
insertHeadType el (TypeNot n) = TypeNot $ insertHeadType el n
insertHeadType el (TypeComp c) = TypeComp $ insertHeadType el c
insertHeadType el (TypeList l) = TypeList (el:l)
insertHeadType el p@(_ :#* _) = el :#* p
insertHeadType el s@(_ :#+ _) = el :#+ s
insertHeadType el t = el :#* t

headType :: TypeTag -> TypeTag
headType (TypeNot n) = headType n
headType (TypeComp c) = headType c
headType (TypeList l) = head l
headType (h :#* _) = h
headType (h :#+ _) = h
headType t = t

tailType :: TypeTag -> TypeTag
tailType (TypeNot n) = TypeNot $ tailType n
tailType (TypeComp c) = TypeComp $ tailType c
tailType (TypeList l) = TypeList $ tail l
tailType (_ :#* t) = t
tailType (_ :#+ t) = t
tailType t = t

findType :: (TypeTag -> Bool) -> TypeTag -> Maybe TypeTag
findType f (TypeNot n) = findType f n
findType f (TypeComp c) = findType f c
findType f (TypeList l) = find f l
findType f (el :#* ts)
    | f el = return el
    | otherwise = findType f ts

findType f (el :#+ ts)
    | f el = return el
    | otherwise = findType f ts

findType f el
    | f el = return el
    | otherwise = Nothing

delElemType :: (TypeTag -> Bool) -> TypeTag -> TypeTag
delElemType f (TypeNot n) = TypeNot $ delElemType f n
delElemType f (TypeComp c) = TypeComp $ delElemType f c
delElemType f (TypeList l) = TypeList $ delElem f l
delElemType f (el :#* ts)
    | f el = ts
    | otherwise = el :#* delElemType f ts

delElemType f (el :#+ ts)
    | f el = ts
    | otherwise = el :#+ delElemType f ts

delElemType f el
    | f el = TypeVoid
    | otherwise = el

deepLengthType :: TypeTag -> Int
deepLengthType = nestedLength
    
    where nestedLength len@(_:#*_) = operLength 0 len
          nestedLength len@(_:#+_) = operLength 0 len
          nestedLength len = constLength len
          
          constLength (TypeNot n) = nestedLength n
          constLength (TypeComp c) = nestedLength c
          constLength (TypeList l) = length l
          constLength _ = 1

          operLength num (_ :#* t) = operLength (succ num) t
          operLength num (_ :#+ t) = operLength (succ num) t
          operLength num _ = succ num

lengthType :: TypeTag -> Int
lengthType = nestedLength
    
    where nestedLength len@(_:#*_) = operLength 0 len
          nestedLength len@(_:#+_) = operLength 0 len
          nestedLength len = 1
          
          operLength num (_ :#* t) = operLength (succ num) t
          operLength num (_ :#+ t) = operLength (succ num) t
          operLength num _ = succ num

substType :: TypeTag -> (TypeTag -> Bool) -> TypeTag -> TypeTag
substType el log (x :#* xs)
    | log x = el :#* xs
    | otherwise = x :#* substType el log xs

substType el log (x :#+ xs)
    | log x = el :#+ xs
    | otherwise = x :#+ substType el log xs

substType el log x
    | log x = el
    | otherwise = x


isTypeList :: TypeTag -> Bool
isTypeList (TypeList {}) = True
isTypeList _ = False

isTypeBool :: TypeTag -> Bool
isTypeBool (TypeBool {}) = True
isTypeBool _ = False

isTypeInt :: TypeTag -> Bool
isTypeInt (TypeInt {}) = True
isTypeInt _ = False


subst :: a -> (a -> Bool) -> [a] -> [a]
subst _ _ [] = []
subst el log (x:xs) | log x = el : xs
subst el log (x:xs) = x : subst el log xs


move :: Bool -> (a -> Bool) -> [a] -> [a]
move direct logElem l = if isJust mElemMove
    then elemMove : list
    else list

    where (mElemMove, list) = nestedMove Nothing l
          elemMove = fromJust mElemMove
        
          nestedMove (Just oldX) [] = (Nothing, [oldX])
          nestedMove _ [] = (Nothing, [])
          nestedMove (Just oldX) (x:xs) = (Nothing, x : oldX : xs)
          nestedMove _ (x:xs)
            | logElem x =
                let (_, futureXS) = nestedMove (Just x) xs 
                in if direct
                    then (Nothing, futureXS)
                    else (Just x, xs)
            | otherwise =
                let (mFutureX, futureXS) = nestedMove Nothing xs
                    futureX = fromJust mFutureX
                in if isJust mFutureX
                    then (Nothing, futureX : x : futureXS)
                    else (Nothing, x : futureXS)


cut :: Bool -> (a -> Bool) -> [a] -> [a]
cut direct logElem l = list

    where (_, list) = nestedCut False l
        
          nestedCut _ [] = (False, [])
          nestedCut True (_:xs) = (False, xs)
          nestedCut _ xxs@(x:xs)
            | logElem x =
                let (_, futureXS) = nestedCut True xs 
                in if direct
                    then (False, x : futureXS)
                    else (True, xxs)
            | otherwise =
                let (futureLog, futureXS) = nestedCut False xs
                in if futureLog
                    then (False, futureXS)
                    else (False, x : futureXS)
 