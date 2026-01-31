{-# LANGUAGE FlexibleInstances{-, Strict-} #-}

module Abacus where

import TypeAbacus
import PureFunctions ( funcIf, mZip3, updFstTuple3, updThrdTuple3, updSndTuple2, effFromRight, tup3InTup2, effFromLeft )
import ImperativeAbacus
import TypeImperativeAbacus ()
import AbacusAPI

import System.Random ( StdGen, Random(randomR) )
import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes)
import Text.Printf (printf)
import Data.Tuple.Extra ( fst3, snd3, thd3 )
import Data.Either (isRight, isLeft)



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

    | isLeft result || (not . operator . fst3 . effFromRight) result && abacus < (snd3 . effFromRight) result =

        let updResult :: Either String (ExprAbacus, [RowAbacus], StdGen) -> Either String (ExprAbacus, Int)
            updResult (Left str) = Left str
            updResult (Right tuple) = Right $ updSndTuple2 (abacusInNum . clearVoidRows) $ tup3InTup2 tuple

            optionStr =
                let yes = "Error Abacus newAbacusAndGen: Абакус состояния (%d) меньше нового (%s):\nbalanceMaxAbacP[%d, maxP[%d] - baseAbacusNum[%d]] < minP[%d]\n(baseAbacusNum[%d] - minM[%d]) < minM[%d]"
                    no = "Error Abacus newAbacusAndGen: Вот абакус состояния (%d) и новый абакус (%s):\nbalanceMaxAbacP[%d, maxP[%d] - baseAbacusNum[%d]] < minP[%d]\n(baseAbacusNum[%d] - minM[%d]) < minM[%d]"
                in if isRight result then yes else no

            errStr = printf optionStr
                (abacusInNum $ clearVoidRows abacus)
                (show $ updResult result)

                balanceMaxAbacP
                maxP
                baseAbacusNum
                minP

                baseAbacusNum
                minM
                minM
            recurErrStr = if isLeft result
                then errStr ++ "\n\n" ++ effFromLeft result
                else errStr
        
        in error recurErrStr

    | otherwise = effFromRight result
    
    where result :: Either String (ExprAbacus, [RowAbacus], StdGen)
          result
            | operator expr = if balanceMaxAbacP < minP
                then minus
                else plus
            | otherwise = if (baseAbacusNum - minM) < minM
                then plus
                else minus
        
          exprP = expr {operator = True}
          exprM = expr {operator = False}

          plus = if isRight eitAbPlusGen
            then return (exprP, curAbacus'P, newGen'P)
            else Left $ effFromLeft eitAbPlusGen

          minus = if isRight eitAbMinusGen
            then return (exprM, curAbacus'M, newGen'M)
            else Left $ effFromLeft eitAbPlusGen
        
          baseAbacusNum = abacusInNum $! abacus
          
          Account (minP, maxP) = countRange 0 range --1 range
          balanceMaxAbacP = maxP - baseAbacusNum
          rangeP = Account (minP, balanceMaxAbacP)
          eitAbPlusGen = randomAbacus rangeP gen $ return (exprP, abacus)
          Right (curAbacus'P, newGen'P) = eitAbPlusGen
        
          Account (minM, maxM) = countRange 0 range
          balanceMaxM = baseAbacusNum
          rangeM = Account (minM, balanceMaxM)
          eitAbMinusGen = randomAbacus rangeM gen $ return (exprM, abacus)
          Right (curAbacus'M, newGen'M) = eitAbMinusGen


exprInSimula :: ExprAbacus -> Simula
exprInSimula expr
    | operator expr = Plus
    | otherwise = Minus
