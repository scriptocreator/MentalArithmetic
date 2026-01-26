{-# LANGUAGE FlexibleInstances{-, Strict-} #-}

module Abacus where

import TypeAbacus
import PureFunctions ( funcIf )
import LazyFunctions
import ImperativeAbacus ( abacusInNum, powerInAbacus, checkAbacus )
import TypeImperativeAbacus (Theme)

import System.Random ( StdGen, Random(randomR) )

infixr 6 --!



instance {-# OVERLAPPING #-} Ord [RowAbacus] where
    leftR > rightR =
        let futureRows = leftR #> rightR
        in case futureRows of
            LT -> False
            EQ -> False
            GT -> True

        where [] #> [] = EQ
              _ #> [] = GT
              [] #> _ = LT

              (RowAbacus leftLower leftUpper:leftRS) #> (RowAbacus rightLower rightUpper:rightRS)
                | not leftUpper && rightUpper = case futureRows of
                    LT -> LT
                    EQ -> LT
                    GT -> GT

                | not (leftUpper || rightUpper) || (leftUpper && rightUpper) =  case futureRows of
                    LT -> LT
                    EQ -> compare natNegLower 0
                    GT -> GT

                -- В случае, если левая 5 присутствует, а правая нет
                | otherwise = case futureRows of
                    LT -> LT
                    EQ -> GT
                    GT -> GT

                where futureRows = leftRS #> rightRS
                      numLeftLower = length leftLower
                      numRightLower = length rightLower
                      natNegLower = numLeftLower - numRightLower

    leftRow < rightRow = not $ leftRow > rightRow

    leftRow >= rightRow = (leftRow == rightRow) || (leftRow > rightRow)
    leftRow <= rightRow = (leftRow == rightRow) || (leftRow < rightRow)


instance Num [RowAbacus] where
    [] + right = right
    left + [] = left

    (RowAbacus leftLower leftUpper:ls) + (RowAbacus rightLower rightUpper:rs)
        | length lower > 4 =
            funcIf (leftUpper && rightUpper) (RowAbacus balance True : return (RowAbacus [Done] False) + ls + rs) $
            funcIf (leftUpper || rightUpper) (RowAbacus balance False : return (RowAbacus [Done] False) + ls + rs) $
            (RowAbacus balance True : ls + rs)

        | otherwise =
            funcIf (leftUpper && rightUpper) (RowAbacus lower False : return (RowAbacus [Done] False) + ls + rs) $
            funcIf (leftUpper || rightUpper) (RowAbacus lower True : ls + rs) $
            (RowAbacus lower False : ls + rs)

        where lower = leftLower ++ rightLower
              balance = drop 5 lower

    [] - [] = []
    left - [] = left
    [] - right = error "Error Abacus №1: Вычитать больший абакус нельзя"

    (RowAbacus leftLower leftUpper:ls) - (RowAbacus rightLower rightUpper:rs)
        | not leftUpper && rightUpper =
            let newls = balanceRow ls
            in if negLower < 0
                then RowAbacus balanceNegBone False : newls - rs
                else RowAbacus balanceBone True : newls - rs

        | not (leftUpper || rightUpper) || (leftUpper && rightUpper) =
            let newls = balanceRow ls
            in if negLower < 0
                then RowAbacus balanceNegBone True : newls - rs
                else RowAbacus balanceBone False : ls - rs

        -- В случае, если левая 5 присутствует, а правая нет
        | otherwise =
            (\cur -> cur : ls - rs) $ if negLower < 0
                then RowAbacus balanceNegBone False
                else RowAbacus balanceBone True

        where numLeftLower = length leftLower
              numRightLower = length rightLower
              negLower = numLeftLower - numRightLower
              balanceNegBone = replicate (5 - abs negLower) Done
              balanceBone = leftLower --! rightLower


balanceRow :: [RowAbacus] -> [RowAbacus]
balanceRow [] = error "Error Abacus №2: У абакуса обнаружены пустые разряды"
balanceRow (RowAbacus (_:lowers) upper:rows) = RowAbacus lowers upper : rows
balanceRow (RowAbacus [] True:rows) = RowAbacus [Done, Done, Done, Done] False : rows
balanceRow (_:rows) = RowAbacus [Done, Done, Done, Done] True : balanceRow rows

(--!) :: [Done] -> [Done] -> [Done]
[] --! [] = []
[] --! _ = error "Error Abacus N3: Для натурального счёта, вычитается большее число косточек"
a --! [] = a
(Done:fis) --! (Done:sis) = fis --! sis


curMerelyAbacus :: StdGen -> [RowAbacus] -> (Int, Int) {--> Theme-} -> [Expr] -> ([Abacus], StdGen)
curMerelyAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curMerelyAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curMerelyAbacus newGen newStandAbacus tupleRange fs
        in (operExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where logExpr = operator expr
          ((+|-), f, operExpr) = if logExpr
            then ((+), newMerelyAbacusPlus, Plus)
            else ((-), newMerelyAbacusMinus, Minus)
           -- Создание текущего аргумента @curAbacus, по диапазонам и теме
          (curAbacus, newGen) = newAbacusAndGen abacus tupleRange operExpr f gen
          -- Применение операции @randomExpr на аргументах
          newStandAbacus = abacus +|- curAbacus

curBrotherAbacus :: StdGen -> [RowAbacus] -> (Int, Int) {--> Theme-} -> [Expr] -> ([Abacus], StdGen)
curBrotherAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curBrotherAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curBrotherAbacus newGen newStandAbacus tupleRange fs
        in (operExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where (logExpr, logPlus) = (operator expr, ifPlus expr)
          ((+|-), f, operExpr) = if logExpr
            then ((+), newBrotherAbacusPlus, Plus)
            else ((-), newBrotherAbacusMinus, Minus)
          (curAbacus, newGen) = newAbacusAndGen abacus tupleRange operExpr (f logPlus) gen
          newStandAbacus = abacus +|- curAbacus

curFriendAbacus :: StdGen -> [RowAbacus] -> (Int, Int) {--> Theme-} -> [Expr] -> ([Abacus], StdGen)
curFriendAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curFriendAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curFriendAbacus newGen newStandAbacus tupleRange fs
        in (operExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where logExpr = operator expr
          ((+|-), f, operExpr) = if logExpr
            then ((+), newFriendAbacusPlus, Plus)
            else ((-), newFriendAbacusMinus, Minus)
          (curAbacus, newGen) = newAbacusAndGen abacus tupleRange operExpr f gen
          newStandAbacus = abacus +|- curAbacus


newAbacusAndGen :: [RowAbacus]
                -> (Int, Int)
                -- -> Theme
                -> Abacus
                -> (RowAbacus -> (Int, Int))
                -> StdGen
                -> ([RowAbacus], StdGen)

newAbacusAndGen abacus tupleRange operExpr f curGen =
    let (dirtCurAbacus, newGen) = dirtyAbacusAndGen curGen tupleRange f abacus
        curAbacus = clearVoidRows dirtCurAbacus

    in if abacusInNum curAbacus == 0 || (operExpr == Minus && abacus < curAbacus) -- || checkAbacus abacus curAbacus theme operExpr
        then newAbacusAndGen abacus tupleRange operExpr f newGen
        else (curAbacus, newGen)


dirtyAbacusAndGen :: StdGen
                  -> (Int, Int)
                  -> (RowAbacus -> (Int, Int))
                  -> [RowAbacus]
                  -> ([RowAbacus], StdGen)

dirtyAbacusAndGen gen tupleRange f abacus = (pureLazy, finalGen)

    where (lenCurAbacus, firstGen) = randomR tupleRange gen
        
          pureLazy = fmap fst dirtLazy
          finalGen = snd $ last dirtLazy
          dirtLazy = take lenCurAbacus $ nestedDirty firstGen abacus

          nestedDirty gen [] = []
          nestedDirty gen (row:as) =
            let ready@(curRow, secondGen) = powerInAbacus gen $ f row
            in (ready : nestedDirty secondGen as)


newMerelyAbacusPlus :: RowAbacus -> (Int, Int)
newMerelyAbacusPlus (RowAbacus lower upper)
    | upper = (powerLower, 0)
    | otherwise = (powerLower, 1)
    where powerLower = 4 - length lower

newBrotherAbacusPlus :: Bool -> RowAbacus -> (Int, Int)
newBrotherAbacusPlus log (RowAbacus lower upper)
    | upper = (powerLower, 0)
    | otherwise = if log
        then (powerLower, 1)
        else (4, 0)
    where powerLower = 4 - length lower

-- Потом допишу «Друг + Брат»
newFriendAbacusPlus :: RowAbacus -> (Int, Int)
newFriendAbacusPlus (RowAbacus lower upper) = (4, 1)


newMerelyAbacusMinus :: RowAbacus -> (Int, Int)
newMerelyAbacusMinus (RowAbacus lower upper)
    | upper = (powerLower, 1)
    | otherwise = (powerLower, 0)
    where powerLower = length lower

newBrotherAbacusMinus :: Bool -> RowAbacus -> (Int, Int)
newBrotherAbacusMinus log (RowAbacus lower upper)
    | upper = (powerLower, 1)
    | otherwise = (powerLower, 0)
    where powerLower = length lower

-- Потом допишу «Друг + Брат»
newFriendAbacusMinus :: RowAbacus -> (Int, Int)
newFriendAbacusMinus (RowAbacus lower upper) = (4, 1)

newAbacus :: (Int, Int)
newAbacus = (4, 1)


clearVoidRows :: [RowAbacus] -> [RowAbacus]
clearVoidRows rows = snd $ nestedClear rows

    where nestedClear :: [RowAbacus] -> (Bool, [RowAbacus])
          nestedClear [] = (True, [])

          nestedClear (row@(RowAbacus [] False):rows) =
            let (futureLogVoid, futureRows) = nestedClear rows
            in if futureLogVoid
                then (True, [])
                else (False, row : futureRows)

          nestedClear (row:rows) =
            let (_, futureRows) = nestedClear rows
            in (False, row : futureRows)
