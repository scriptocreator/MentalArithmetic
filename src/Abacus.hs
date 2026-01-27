{-# LANGUAGE FlexibleInstances{-, Strict-} #-}

module Abacus where

import TypeAbacus
import PureFunctions ( funcIf, getLast )
import LazyFunctions
import ImperativeAbacus ( abacusInNum, powerInAbacus, checkAbacus, numInAbacus )
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
    [] - right = error "Error Abacus (-): Вычитать больший абакус нельзя"

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
balanceRow [] = error "Error Abacus balanceRow: У абакуса обнаружены пустые разряды"
balanceRow (RowAbacus (_:lowers) upper:rows) = RowAbacus lowers upper : rows
balanceRow (RowAbacus [] True:rows) = RowAbacus [Done, Done, Done, Done] False : rows
balanceRow (_:rows) = RowAbacus [Done, Done, Done, Done] True : balanceRow rows

(--!) :: [Done] -> [Done] -> [Done]
[] --! [] = []
[] --! _ = error "Error Abacus (--!): Для натурального счёта, вычитается большее число косточек"
a --! [] = a
(Done:fis) --! (Done:sis) = fis --! sis


curMerelyAbacus :: StdGen -> [RowAbacus] -> (Int, Int) {--> Theme-} -> [Expr] -> ([Abacus], StdGen)
curMerelyAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curMerelyAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curMerelyAbacus newGen newStandAbacus tupleRange fs
        in (curOperExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where logExpr = operator expr
          ((+|-), f, operExpr) = if logExpr
            then ((+), newMerelyAbacusPlus, Plus)
            else ((-), newMerelyAbacusMinus, Minus)
           -- Создание текущего аргумента @curAbacus и оператора @curOperExpr, по диапазонам и теме
          (curOperExpr, curAbacus, newGen) = newAbacusAndGen abacus operExpr tupleRange f gen
          -- Применение операции @(+|=) на аргументах
          newStandAbacus = abacus +|- curAbacus

curBrotherAbacus :: StdGen -> [RowAbacus] -> (Int, Int) {--> Theme-} -> [Expr] -> ([Abacus], StdGen)
curBrotherAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curBrotherAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curBrotherAbacus newGen newStandAbacus tupleRange fs
        in (curOperExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where (logExpr, logPlus) = (operator expr, ifPlus expr)
          ((+|-), f, operExpr) = if logExpr
            then ((+), newBrotherAbacusPlus, Plus)
            else ((-), newBrotherAbacusMinus, Minus)
          (curOperExpr, curAbacus, newGen) = newAbacusAndGen abacus operExpr tupleRange (f logPlus) gen
          newStandAbacus = abacus +|- curAbacus

curFriendAbacus :: StdGen -> [RowAbacus] -> (Int, Int) {--> Theme-} -> [Expr] -> ([Abacus], StdGen)
curFriendAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curFriendAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curFriendAbacus newGen newStandAbacus tupleRange fs
        in (curOperExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where logExpr = operator expr
          ((+|-), f, operExpr) = if logExpr
            then ((+), newFriendAbacusPlus, Plus)
            else ((-), newFriendAbacusMinus, Minus)
          (curOperExpr, curAbacus, newGen) = newAbacusAndGen abacus operExpr tupleRange f gen
          newStandAbacus = abacus +|- curAbacus


{-| Я заменил версию с использованием @powerInAbacus,
  | на использование @numInAbacus.
  |
  | Это означает, что теперь могут возникать ошибки счёта,
  | так как я полностью игнорирую правила перехода через 5 и 10.
  | Ещё возможна ошибка вычитания большего.

  | Но это необходимо, поскольку «стрелять наугад» в правильное
  | число – не вариант, лучше уж сразу его правильно создать.
  | Я уже узнал как это сделать:
  | • Надо создать список минимальных и максимальных потенциалов
  |   разрядов, и перевернуть его [у меня единицы идут сначала].
  |   Ну и добавить потенциалы темы, операции, и абакуса состояния
  |   конечно!
  | • Потом, передавать минимальное и максимальное число в каждом
  |   разряде, создавать новое, с новым разрядом в рамках этого
  |   диапазона, и его числа пределы (пару штук), на 00 и 99 для
  |   меньших разрядов соответсвенно.
  | • После, сравнивать их и предыдущие через `min` и `max`,
  |   и так дойти до единиц.
  |-}
newAbacusAndGen :: [RowAbacus]
                -> Abacus
                -> (Int, Int)
                -- -> Theme
                -> (RowAbacus -> (Int, Int))
                -> StdGen
                -> (Abacus, [RowAbacus], StdGen)

newAbacusAndGen abacus operExpr range@(nMinRange, nMaxRange) f gen = 
    case operExpr of
        Minus -> if checkM < simulaMin'M
            then plus
            else minus

        Plus -> if balanceMax'P < simulaMin'P
            then minus
            else plus

        _ -> error "Error Abacus newAbacusAndGen: Нарушена цепочка «Абакус Оператор Абакус»"

    where minus = (Minus, numInAbacus curNumAbacus'M, newGen'M)
          plus = (Plus, numInAbacus curNumAbacus'P, newGen'P)
        
          baseAbacusNum = abacusInNum $! abacus
          (absMinCountRows, absMaxCountRows) = (abs nMinRange, abs nMaxRange)
          funcSimula numOp =
            let newMinCountRows = pred absMinCountRows
            in (if newMinCountRows <= 0 then numOp else 10 ^ newMinCountRows, pred $ 10 ^ absMaxCountRows) :: (Int, Int)
          
          (simulaMin'P, simulaMax'P) = funcSimula 1
          balanceMax'P = simulaMax'P - baseAbacusNum
          rangeP = (simulaMin'P, balanceMax'P)
          (curNumAbacus'P, newGen'P) = randomR rangeP gen
        
          (simulaMin'M, simulaMax'M) = funcSimula 0
          balanceMax'M = baseAbacusNum
          rangeM = (simulaMin'M, balanceMax'M)
          (curNumAbacus'M, newGen'M) = randomR rangeM gen
          checkM = balanceMax'M - simulaMin'M


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
