{-# LANGUAGE Strict #-}

module PureFunctions where

import TypeAbacus
import Text.Printf (printf)
import Data.List (find)


fromRight :: Either a b -> b
fromRight (Right r) = r

fromLeft :: Either a b -> a
fromLeft (Left l) = l


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
          nestedSort sortList (t:ls) = nestedSort (setTypeList t sortList) ls

setTypeList :: TypeTag -> [TypeTag] -> [TypeTag]
setTypeList t [] = [t]
setTypeList t (tel:ls)
    | headType t == headType tel
        = error $ printf "Error PureFunctions №1: Обнаружена ещё одна переменная под аргумент конструктора – «%s» и «%s»" (show arg1) (show arg2)
    | headType t < headType tel = t : tel : ls
    | otherwise = tel : setTypeList t ls

    where arg1 = headType $ tailType t
          arg2 = headType $ tailType tel 


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
insertHeadType el (TypeNot n) = insertHeadType el n
insertHeadType el (TypeComp c) = insertHeadType el c
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
