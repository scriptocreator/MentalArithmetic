module PureFunctions where



delElem :: (a -> Bool) -> [a] -> [a]
delElem _ [] = []
delElem f (x:xs) | f x = xs
delElem f (x:xs) = x : delElem f xs

splitList :: Int -> [a] -> [[a]]
splitList divStand = nestedSplit 1 []

  where nestedSplit :: Int -> [a] -> [a] -> [[a]]
        nestedSplit _ interim [] = [interim]
        nestedSplit st interim (x:xs) | st == divStand =
          let predInterim = interim++return x
          in predInterim : nestedSplit (succ st) [] xs
        nestedSplit st interim (x:xs) =
          nestedSplit (succ st) (interim++return x) xs
