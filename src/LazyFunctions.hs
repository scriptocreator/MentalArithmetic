module LazyFunctions where

import TypeAbacus

import System.Random



repeatExpr :: StdGen -> [(Expr, StdGen)]
repeatExpr gen = (curOper, firstGen) : repeatExpr firstGen

    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          curOper = Expr (numOper /= 0)

repeatExprBro :: StdGen -> [(Expr, StdGen)]
repeatExprBro gen = (curOper, secondGen) : repeatExprBro firstGen

    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          (numPlus, secondGen) = randomR ((0, 1) :: (Int, Int)) firstGen
          curOper = ExprBro (numOper /= 0) (numPlus /= 0)
