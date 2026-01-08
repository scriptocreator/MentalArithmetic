{-# LANGUAGE FlexibleInstances, Strict #-}

module Abacus where

import TypeAbacus
import ImperativeAbacus
import System.Random.Stateful

infixr 6 --!



instance Eq RowAbacus where
    RowAbacus leftLower leftUpper == RowAbacus rightLower rightUpper
        | (leftLower == rightLower) && (leftUpper == rightUpper) = True
        | otherwise = False

    leftRow /= rightRow = not $ leftRow == rightRow

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
    [] - right = error "Error №1: Вычитать больший абакус нельзя"

    (RowAbacus leftLower leftUpper:ls) - (RowAbacus rightLower rightUpper:rs)
        | not leftUpper && rightUpper =
            let newrs = balanceRow rs
            in if negLower < 0
                then RowAbacus balanceBone True : ls - newrs
                else RowAbacus balanceNegBone False : ls - newrs

        | not (leftUpper || rightUpper) || (leftUpper && rightUpper) = if negLower < 0
            then let newrs = balanceRow rs
                in RowAbacus balanceNegBone True : ls - newrs
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


balanceRow [] = error "Error №2: У абакуса обнаружены пустые разряды"
balanceRow (RowAbacus (_:lowers) upper:rows) = RowAbacus lowers upper : rows
balanceRow (RowAbacus [] True:rows) = RowAbacus [Done, Done, Done, Done] False : rows
balanceRow (_:rows) = RowAbacus [Done, Done, Done, Done] True : balanceRow rows

funcIf log a b = if log then a else b

(--!) :: [Done] -> [Done] -> [Done]
[] --! _ = []
a --! [] = a
(Done:fis) --! (Done:sis) = fis --! sis


curMerelyAbacus :: StdGen -> [RowAbacus] -> [App] -> ([Abacus], StdGen)
curMerelyAbacus gen abacus [] = ([Abacus abacus], gen)
curMerelyAbacus gen abacus (App logApp f:fs)
    = let (futureAbacusis, futureGen) = curMerelyAbacus newGen newStandAbacus fs
        in (operApp : Abacus curAbacus : futureAbacusis, futureGen)

    where newAbacusAndGen :: StdGen -> [RowAbacus] -> ([RowAbacus], StdGen) -- Создание текущего аргумента, по диапазонам и теме
          newAbacusAndGen gen [] = ([], gen)
          newAbacusAndGen gen (row:as) = let (curRow, firstGen) = powerInAbacus gen $ f row
                                             (futureRows, secondGen) = newAbacusAndGen firstGen as
            in (curRow : futureRows, secondGen)
          (curAbacus, newGen) = newAbacusAndGen gen abacus
          (randomApp, operApp) = if logApp then ((+), Plus) else ((-), Minus)
          newStandAbacus = abacus `randomApp` curAbacus -- Применение операции на аргументах

curBrotherAbacus :: StdGen -> [RowAbacus] -> [AppBro] -> ([Abacus], StdGen)
curBrotherAbacus gen abacus [] = ([Abacus abacus], gen)
curBrotherAbacus gen abacus (AppBro logApp logPlus f:fs)
    = let (futureAbacusis, futureGen) = curBrotherAbacus newGen newStandAbacus fs
        in (operApp : Abacus curAbacus : futureAbacusis, futureGen)

    where newAbacusAndGen :: StdGen -> [RowAbacus] -> ([RowAbacus], StdGen) -- Создание текущего аргумента, по диапазонам и теме
          newAbacusAndGen gen [] = ([], gen)
          newAbacusAndGen gen (row:as) = let (curRow, firstGen) = powerInAbacus gen $ f logPlus row
                                             (futureRows, secondGen) = newAbacusAndGen firstGen as
            in (curRow : futureRows, secondGen)
          (curAbacus, newGen) = newAbacusAndGen gen abacus
          (randomApp, operApp) = if logApp then ((+), Plus) else ((-), Minus)
          newStandAbacus = abacus `randomApp` curAbacus

curFriendAbacus :: StdGen -> [RowAbacus] -> [App] -> ([Abacus], StdGen)
curFriendAbacus gen abacus [] = ([Abacus abacus], gen)
curFriendAbacus gen abacus (App logApp f:fs)
    = let (futureAbacusis, futureGen) = curFriendAbacus newGen newStandAbacus fs
        in (operApp : Abacus curAbacus : futureAbacusis, futureGen)

    where newAbacusAndGen :: StdGen -> [RowAbacus] -> ([RowAbacus], StdGen) -- Создание текущего аргумента, по диапазонам и теме
          newAbacusAndGen gen [] = ([], gen)
          newAbacusAndGen gen (row:as) = let (curRow, firstGen) = powerInAbacus gen $ f row
                                             (futureRows, secondGen) = newAbacusAndGen firstGen as
            in (curRow : futureRows, secondGen)
          (curAbacus, newGen) = newAbacusAndGen gen abacus
          (randomApp, operApp) = if logApp then ((+), Plus) else ((-), Minus)
          newStandAbacus = abacus `randomApp` curAbacus


newMerelyAbacusPlus :: RowAbacus -> (Int, Int)
newMerelyAbacusPlus (RowAbacus lower upper)
    | upper = (powerLower, 0)
    | otherwise = (powerLower, 1)
    where powerLower = 4 - length lower

newBrotherAbacusPlus :: Bool -> RowAbacus -> (Int, Int)
newBrotherAbacusPlus log (RowAbacus lower upper)
    | upper = (4, 0)
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
    where powerLower = 4 - length lower

-- Потом допишу «Друг + Брат»
newFriendAbacusMinus :: RowAbacus -> (Int, Int)
newFriendAbacusMinus (RowAbacus lower upper) = (4, 1)
