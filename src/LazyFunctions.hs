module LazyFunctions where

import TypeAbacus

import System.Random



repeatExpr :: StdGen -> Int -> ([Expr], StdGen)
repeatExpr gen len = (pureLazy, finalGen)

    where pureLazy = fmap fst dirtLazy
          finalGen = snd $ last dirtLazy
          dirtLazy = take len $ lazyRepeatExpr gen
        

lazyRepeatExpr :: StdGen -> [(Expr, StdGen)]  
lazyRepeatExpr gen = (curOper, firstGen) : lazyRepeatExpr firstGen
            
    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          curOper = Expr (numOper /= 0)


repeatExprBro :: StdGen -> Int -> ([Expr], StdGen)
repeatExprBro gen len = (pureLazy, finalGen)

    where pureLazy = fmap fst dirtLazy
          finalGen = snd $ last dirtLazy
          dirtLazy = take len $ lazyRepeatExprBro gen

        
lazyRepeatExprBro :: StdGen -> [(Expr, StdGen)]
lazyRepeatExprBro gen = (curOper, secondGen) : lazyRepeatExprBro firstGen

    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          (numPlus, secondGen) = randomR ((0, 1) :: (Int, Int)) firstGen
          curOper = ExprBro (numOper /= 0) (numPlus /= 0)
