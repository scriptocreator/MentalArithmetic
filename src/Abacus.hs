{-# LANGUAGE FlexibleInstances, Strict #-}

module Abacus where

import TypeAbacus
import ImperativeAbacus

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
        | (not leftUpper) && rightUpper =
            let newrs = balanceRow rs
            in if negLower < 0
                then RowAbacus balanceBone True : ls - newrs
                else RowAbacus balanceNegBone False : ls - newrs

        | (not (leftUpper || rightUpper)) || (leftUpper && rightUpper) = if negLower < 0
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
              balanceNegBone = take (5 - abs negLower) $ repeat Done
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


curMerelyAbacus :: Abacus -> [App] -> [Abacus]
curMerelyAbacus abacus [] = [abacus]
curMerelyAbacus abacus (App logApp f:fs) = curAbacus : curMerelyAbacus futureAbacus fs

    where curAbacus = fmap powerInAbacus $ fmap f abacus -- Создание текущего аргумента, по диапазонам и теме
          randomApp = if logApp then (+) else (-)
          futureAbacus = abacus `randomApp` curAbacus -- Применение операции на аргументах

curBrotherAbacus :: Abacus -> [AppBro] -> [Abacus]
curBrotherAbacus abacus [] = [abacus]
curBrotherAbacus abacus (AppBro logApp logPlus f:fs) = curAbacus : curBrotherAbacus futureAbacus fs

    where curAbacus = fmap powerInAbacus $ fmap (f logPlus) abacus
          randomApp = if logApp then (+) else (-)
          futureAbacus = abacus `randomApp` curAbacus

curFriendAbacus :: Abacus -> [App] -> [Abacus]
curFriendAbacus abacus [] = [abacus]
curFriendAbacus abacus (App logApp f:fs) = curAbacus : curFriendAbacus futureAbacus fs

    where curAbacus = fmap powerInAbacus $ fmap f abacus
          randomApp = if logApp then (+) else (-)
          futureAbacus = abacus `randomApp` curAbacus


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
newBrotherAbacusMinus _ (RowAbacus lower upper)
    | upper = (powerLower, 1)
    | otherwise = (powerLower, 0)
    where powerLower = 4 - length lower

-- Потом допишу «Друг + Брат»
newFriendAbacusMinus :: RowAbacus -> (Int, Int)
newFriendAbacusMinus (RowAbacus lower upper) = (4, 1)
