module LazyFunctions where

import TypeAbacus

import System.Random



repeatExpr :: StdGen -> ([Expr], StdGen)
repeatExpr gen = (curOper : futureExpr, finalGen)

    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          (futureExpr, finalGen) = repeatExpr firstGen
          curOper = Expr (numOper /= 0)

repeatExprBro :: StdGen -> ([Expr], StdGen)
repeatExprBro gen = (curOper : futureExpr, finalGen)

    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          (numPlus, secondGen) = randomR ((0, 1) :: (Int, Int)) firstGen
          (futureExpr, finalGen) = repeatExprBro secondGen
          curOper = ExprBro (numOper /= 0) (numPlus /= 0)
