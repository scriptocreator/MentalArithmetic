{-# LANGUAGE Strict #-}

module ImperativeAbacus where

import TypeAbacus


powerInAbacus :: (Int, Int) -> RowAbacus
powerInAbacus (numLower, numUpper) = RowAbacus (take numLower $ repeat Done) (if numUpper == 0 then False else True)
{-
powerInAbacus (numLower, numUpper) = RowAbacus newLower newUpper

    where randomLower = 
          randomUpper = 
          newLower = repeat randomLower Identity
          newUpper = if null numUpper
            then False
            else 
-}