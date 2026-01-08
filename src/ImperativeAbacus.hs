{-# LANGUAGE Strict #-}

module ImperativeAbacus where

import TypeAbacus

import System.Random
import System.Random.Stateful

infixr 6 :\^/


data World
    = VoidWorld StdGen
    | VoidSettings StartLine

    | World Expression StdGen Carriage
    | Settings StartLine (Maybe LengthExpr) (Maybe Theme) (Maybe RangeRows)

    | World :\^/ World


powerInAbacus :: StdGen -> (Int, Int) -> (RowAbacus, StdGen)
powerInAbacus gen (numLower, numUpper) = (RowAbacus newLower newUpper, finalGen)

    where (randomLower, firstGen) = randomR (0, numLower) gen
          newLower = replicate randomLower Done

          (newUpper, finalGen) = if numUpper == 0
            then (False, firstGen)
            else let (num, secondGen) = randomR (0, numUpper) firstGen
                     curUpper = (num /= 0)
              in (curUpper, secondGen)
--powerInAbacus (numLower, numUpper) = RowAbacus (take numLower $ repeat Done) (if numUpper == 0 then False else True)

abacusInNum :: [RowAbacus] -> Int
abacusInNum abacus = internalNum $ zip [1,2..] abacus

    where internalNum [] = 0
          internalNum ((row, RowAbacus lower upper):abacusis) = let numUpper = if upper then 5 * row else 0
            in length lower * row + numUpper + internalNum abacusis

exprAbacusInList :: [Abacus] -> [String]
exprAbacusInList [] = []
exprAbacusInList (Plus:as) = " + " : exprAbacusInList as
exprAbacusInList (Minus:as) = " - " : exprAbacusInList as
exprAbacusInList (Abacus abacus:as) = show (abacusInNum abacus) : exprAbacusInList as

splitList :: Int -> [a] -> [[a]]
splitList divStand = nestedSplit 1 []

  where nestedSplit :: Int -> [a] -> [a] -> [[a]]
        nestedSplit _ interim [] = [interim]
        nestedSplit st interim (x:xs) | st == divStand =
          let predInterim = interim++return x
          in predInterim : nestedSplit (succ st) [] xs
        nestedSplit st interim (x:xs) =
          nestedSplit (succ st) (interim++return x) xs
