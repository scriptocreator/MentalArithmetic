{-# LANGUAGE FlexibleInstances{-, Strict-} #-}

module Abacus where

import TypeAbacus
import PureFunctions ( funcIf, mZip3, updFstTuple3, updThrdTuple3, updSndTuple2 )
import ImperativeAbacus
import TypeImperativeAbacus ()
import AbacusAPI

import System.Random ( StdGen, Random(randomR) )
import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes)
import Text.Printf (printf)
import Data.Tuple.Extra ( fst3, snd3, thd3 )



curMerelyAbacus :: StdGen -> [RowAbacus] -> Amount (Int, Int) {--> Theme-} -> [ExprAbacus] -> ([Simula], StdGen)
curMerelyAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curMerelyAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curMerelyAbacus newGen newStandAbacus tupleRange fs
        in (exprInSimula curExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where (+|-) = if operator curExpr then (+) else (-)
           -- Создание текущего аргумента @curAbacus и оператора @curOperExpr, по диапазонам и теме
          (curExpr, curAbacus, newGen) = newAbacusAndGen abacus expr tupleRange gen
          -- Применение операции @(+|=) на аргументах
          dirtNewStandAbacus = abacus +|- curAbacus
          newStandAbacus = clearVoidRows dirtNewStandAbacus

curBrotherAbacus :: StdGen -> [RowAbacus] -> Amount (Int, Int) {--> Theme-} -> [ExprAbacus] -> ([Simula], StdGen)
curBrotherAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curBrotherAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curBrotherAbacus newGen newStandAbacus tupleRange fs
        in (exprInSimula curExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where (+|-) = if operator curExpr then (+) else (-)
          (curExpr, curAbacus, newGen) = newAbacusAndGen abacus expr tupleRange gen
          dirtNewStandAbacus = abacus +|- curAbacus
          newStandAbacus = clearVoidRows dirtNewStandAbacus

curFriendAbacus :: StdGen -> [RowAbacus] -> Amount (Int, Int) {--> Theme-} -> [ExprAbacus] -> ([Simula], StdGen)
curFriendAbacus gen abacus tupleRange [] = (Equal : [Abacus abacus], gen)
curFriendAbacus gen abacus tupleRange (expr:fs)
    = let (futureAbacusis, futureGen) = curFriendAbacus newGen newStandAbacus tupleRange fs
        in (exprInSimula curExpr : Abacus curAbacus : futureAbacusis, futureGen)

    where (+|-) = if operator curExpr then (+) else (-)
          (curExpr, curAbacus, newGen) = newAbacusAndGen abacus expr tupleRange gen
          dirtNewStandAbacus = abacus +|- curAbacus
          newStandAbacus = clearVoidRows dirtNewStandAbacus


newAbacusAndGen :: [RowAbacus]
                -> ExprAbacus
                -> Amount (Int, Int)
                -> StdGen
                -> (ExprAbacus, [RowAbacus], StdGen)

newAbacusAndGen abacus expr range gen
    | (not . operator . fst3) result && abacus < snd3 result = error $ printf
        "Error Abacus newAbacusAndGen: Абакус состояния (%s) меньше нового (%s):\nbalanceMaxAbacP[%d, maxP[%d] - baseAbacusNum[%d]] < minP[%d]\n(baseAbacusNum[%d] - minM[%d]) < minM[%d]"
        (show abacus)
        (show $ snd3 result)

        balanceMaxAbacP
        maxP
        baseAbacusNum
        minP

        baseAbacusNum
        minM
        minM

    | otherwise = result
    
    where result
            | operator expr = if balanceMaxAbacP < minP
                then minus
                else plus
            | otherwise = if (baseAbacusNum - minM) < minM
                then plus
                else minus
        
          exprP = expr {operator = True}
          exprM = expr {operator = False}
          plus = (exprP, curAbacus'P, newGen'P)
          minus = (exprM, curAbacus'M, newGen'M)
        
          baseAbacusNum = abacusInNum $! abacus
          
          Account (minP, maxP) = countRange 0 range --1 range
          balanceMaxAbacP = maxP - baseAbacusNum
          rangeP = Account (minP, balanceMaxAbacP)
          (curAbacus'P, newGen'P) = randomAbacus rangeP gen $ return (exprP, abacus)
        
          Account (minM, maxM) = countRange 0 range
          balanceMaxM = baseAbacusNum
          rangeM = Account (minM, balanceMaxM)
          (curAbacus'M, newGen'M) = randomAbacus rangeM gen $ return (exprM, abacus)


exprInSimula :: ExprAbacus -> Simula
exprInSimula expr
    | operator expr = Plus
    | otherwise = Minus
