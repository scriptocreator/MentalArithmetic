{-# LANGUAGE FlexibleInstances{-, Strict-} #-}

module Abacus where

import TypeAbacus
import PureFunctions
import LazyFunctions
import ImperativeAbacus

import System.Random.Stateful ( StdGen )

infixr 6 --!



instance Ord RowAbacus where
    RowAbacus leftLower True > RowAbacus rightLower False = True
    RowAbacus leftLower False > RowAbacus rightLower True = False

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


curMerelyAbacus :: StdGen -> [RowAbacus] -> [Expr] -> ([Abacus], StdGen)
curMerelyAbacus gen abacus [] = ([Abacus abacus], gen)
curMerelyAbacus gen abacus (expr:fs)
    = let (futureAbacusis, futureGen) = curMerelyAbacus newGen newStandAbacus fs
        in (operExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where logExpr = operator expr
          (randomExpr, f, operExpr) = if logExpr
            then ((+), newMerelyAbacusPlus, Plus)
            else ((-), newMerelyAbacusMinus, Minus)
           -- Создание текущего аргумента @curAbacus, по диапазонам и теме
          (curAbacus, newGen) = newAbacusAndGen gen f abacus
          -- Применение операции @randomExpr на аргументах
          newStandAbacus = abacus `randomExpr` curAbacus

curBrotherAbacus :: StdGen -> [RowAbacus] -> [Expr] -> ([Abacus], StdGen)
curBrotherAbacus gen abacus [] = ([Abacus abacus], gen)
curBrotherAbacus gen abacus (expr:fs)
    = let (futureAbacusis, futureGen) = curBrotherAbacus newGen newStandAbacus fs
        in (operExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where (logExpr, logPlus) = (operator expr, ifPlus expr)
          (randomExpr, f, operExpr) = if logExpr
            then ((+), newBrotherAbacusPlus, Plus)
            else ((-), newBrotherAbacusMinus, Minus)
          (curAbacus, newGen) = newAbacusAndGen gen (f logPlus) abacus
          newStandAbacus = abacus `randomExpr` curAbacus

curFriendAbacus :: StdGen -> [RowAbacus] -> [Expr] -> ([Abacus], StdGen)
curFriendAbacus gen abacus [] = ([Abacus abacus], gen)
curFriendAbacus gen abacus (expr:fs)
    = let (futureAbacusis, futureGen) = curFriendAbacus newGen newStandAbacus fs
        in (operExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where logExpr = operator expr
          (randomExpr, f, operExpr) = if logExpr
            then ((+), newFriendAbacusPlus, Plus)
            else ((-), newFriendAbacusMinus, Minus)
          (curAbacus, newGen) = newAbacusAndGen gen f abacus
          newStandAbacus = abacus `randomExpr` curAbacus


newAbacusAndGen :: StdGen -> (RowAbacus -> (Int, Int)) -> [RowAbacus] -> ([RowAbacus], StdGen)
newAbacusAndGen gen _ [] = ([], gen)
newAbacusAndGen gen f (row:as) =
    let (curRow, firstGen) = powerInAbacus gen $ f row
        (futureRows, secondGen) = newAbacusAndGen firstGen f as
    in (curRow : futureRows, secondGen)


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
