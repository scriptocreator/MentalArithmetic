module LazyFunctions where

import TypeAbacus
import PureFunctions (getLast)

import System.Random ( StdGen, Random(randomR) )



repeatExpr :: ExprAbacus -> StdGen -> Int -> ([ExprAbacus], StdGen)
repeatExpr expr gen len = (pureLazy, finalGen)
    where pureLazy = fmap fst dirtLazy
          finalGen = snd $! getLast dirtLazy
          dirtLazy = take len $ lazyRepeatExpr expr gen
        

lazyRepeatExpr :: ExprAbacus -> StdGen -> [(ExprAbacus, StdGen)]  
lazyRepeatExpr expr gen = (curOper, firstGen) : lazyRepeatExpr expr firstGen
            
    where (numOper, firstGen) = randomR ((0, 1) :: (Int, Int)) gen
          curOper =  expr {operator = numOper /= 0}
