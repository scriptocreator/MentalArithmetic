module Main (main) where

import Abacus
import TypeAbacus
import ImperativeAbacus

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromJust)



halfHoriz = 1920 `div` 2
halfVert = 1040 `div` 2
initWord gen = VoidSettings (-halfHoriz) :\^/ VoidWorld gen


picture (VoidWorld _ :\^/ _) = Blank
picture (World exprs _ _ :\^/ Settings start lenExpr _ _) = pictures allPicturies

    where funcSplitLinesAbacus :: [Abacus] -> [String]
          funcSplitLinesAbacus a = fmap unlines $ splitList (fromJust lenExpr) $ exprAbacusInList a

          splitLinesAbacus = fmap funcSplitLinesAbacus exprs

          (_, allPicturies) = foldl outputAbacusis (start, []) splitLinesAbacus

outputAbacusis :: (Int, [Picture]) -> [String] -> (Int, [Picture])
outputAbacusis st [] = st
outputAbacusis (current, predPictures) (str:ss)
    | current < (-halfVert) = (current + 40, predPictures)
    | current > halfVert = outputAbacusis (newCurrent, predPictures ++ [Text str]) ss
    | otherwise = outputAbacusis (newCurrent, predPictures) ss
    where newCurrent = current + 20


handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (VoidWorld gen :\^/ VoidSettings horiz) = (VoidSettings horiz :\^/ VoidWorld gen)
handleKey (EventKey (SpecialKey KeyCtrlL) Down _ _) (world@(World {}) :\^/ set@(Settings {})) = (set :\^/ world)
handleKey _ world = world


update _ world = world


main :: IO ()
main = do
    gen <- newStdGen
    play (InWindow "Random Abacus" (1920, 1040) (0, 0))
         white
         30
         (initWord gen)
         picture
         handleKey
         update
