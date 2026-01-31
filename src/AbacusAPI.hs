{-# LANGUAGE FlexibleInstances{-, Strict-} #-}

module AbacusAPI where

import TypeAbacus
import TypeImperativeAbacus
import PureFunctions

import System.Random ( StdGen, Random(randomR) )
import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes)
import Text.Printf (printf)
import Data.Either (isLeft, isRight)

infixr 6 --!


instance {-# OVERLAPPING #-} Ord [RowAbacus] where
    leftRow > rightRow =
        let futureRows = leftRow #> rightRow
        in case futureRows of
            LT -> False -- Аналог лжи
            GT -> True -- Аналог истины
            EQ -> False

        where [] #> [] = EQ

              (RowAbacus [] False:rs) #> [] = case [] #> rs of
                    LT -> LT
                    GT -> GT
                    EQ -> EQ
                
              [] #> (RowAbacus [] False:rs) = case [] #> rs of
                    LT -> LT
                    GT -> GT
                    EQ -> EQ

              _ #> [] = GT
              [] #> _ = LT

              (RowAbacus leftLower leftUpper:leftRS) #> (RowAbacus rightLower rightUpper:rightRS)
                | leftUpper && not rightUpper = case futureRows of
                    LT -> LT
                    GT -> GT
                    EQ -> GT

                | not (leftUpper || rightUpper) || (leftUpper && rightUpper) =  case futureRows of
                    LT -> LT
                    GT -> GT
                    EQ -> compare natLeftLower 0

                -- В случае, если правая 5 присутствует, а левая нет
                | otherwise = case futureRows of
                    LT -> LT
                    GT -> GT
                    EQ -> LT

                where futureRows = leftRS #> rightRS
                      numLeftLower = length leftLower
                      numRightLower = length rightLower
                      natLeftLower = numLeftLower - numRightLower

    leftRow < rightRow =
        let futureRows = leftRow #< rightRow
        in case futureRows of
            LT -> False -- Аналог лжи
            GT -> True -- Аналог истины
            EQ -> False

        where [] #< [] = EQ

              (RowAbacus [] False:rs) #< [] = case [] #< rs of
                    LT -> LT
                    GT -> GT
                    EQ -> EQ
                
              [] #< (RowAbacus [] False:rs) = case [] #< rs of
                    LT -> LT
                    GT -> GT
                    EQ -> EQ

              _ #< [] = LT
              [] #< _ = GT

              (RowAbacus leftLower leftUpper:leftRS) #< (RowAbacus rightLower rightUpper:rightRS)
                | not leftUpper && rightUpper = case futureRows of
                    LT -> LT
                    GT -> GT
                    EQ -> GT

                | not (leftUpper || rightUpper) || (leftUpper && rightUpper) =  case futureRows of
                    LT -> LT
                    GT -> GT
                    EQ -> compare natNatLower 0

                -- В случае, если левая 5 присутствует, а правая нет
                | otherwise = case futureRows of
                    LT -> LT
                    GT -> GT
                    EQ -> LT

                where futureRows = leftRS #< rightRS
                      numLeftLower = length leftLower
                      numRightLower = length rightLower
                      natNatLower = numRightLower - numLeftLower

    leftRow >= rightRow = (leftRow == rightRow) || (leftRow > rightRow)
    leftRow <= rightRow = (leftRow == rightRow) || (leftRow < rightRow)


instance Num [RowAbacus] where
    [] + right = right
    left + [] = left

    (RowAbacus leftLower leftUpper:ls) + (RowAbacus rightLower rightUpper:rs)
        | length lower > 4 =
            funcIf (leftUpper && rightUpper) (RowAbacus balance True : return (RowAbacus [Done] False) + ls + rs) $
            funcIf (leftUpper || rightUpper) (RowAbacus balance False : return (RowAbacus [Done] False) + ls + rs) $
            (RowAbacus balance True : ls + rs)

        | otherwise =
            funcIf (leftUpper && rightUpper) (RowAbacus lower False : return (RowAbacus [Done] False) + ls + rs) $
            funcIf (leftUpper || rightUpper) (RowAbacus lower True : ls + rs) $
            (RowAbacus lower False : ls + rs)

        where lower = leftLower ++ rightLower
              balance = drop 5 lower

    [] - [] = []
    left - [] = left
    [] - right = error "Error AbacusAPI (-): Вычитать больший абакус нельзя"

    (RowAbacus leftLower leftUpper:ls) - (RowAbacus rightLower rightUpper:rs)
        | not leftUpper && rightUpper =
            let newls = balanceRow ls
            in if negLower < 0
                then RowAbacus balanceNegBone False : newls - rs
                else RowAbacus balanceBone True : newls - rs

        | not (leftUpper || rightUpper) || (leftUpper && rightUpper) =
            let newls = balanceRow ls
            in if negLower < 0
                then RowAbacus balanceNegBone True : newls - rs
                else RowAbacus balanceBone False : ls - rs

        -- В случае, если левая 5 присутствует, а правая нет
        | otherwise =
            (\cur -> cur : ls - rs) $ if negLower < 0
                then RowAbacus balanceNegBone False
                else RowAbacus balanceBone True

        where numLeftLower = length leftLower
              numRightLower = length rightLower
              negLower = numLeftLower - numRightLower
              balanceNegBone = replicate (5 - abs negLower) Done
              balanceBone = leftLower --! rightLower


balanceRow :: [RowAbacus] -> [RowAbacus]
balanceRow [] = error "Error AbacusAPI balanceRow: У абакуса обнаружены пустые разряды"
balanceRow (RowAbacus (_:lowers) upper:rows) = RowAbacus lowers upper : rows
balanceRow (RowAbacus [] True:rows) = RowAbacus [Done, Done, Done, Done] False : rows
balanceRow (_:rows) = RowAbacus [Done, Done, Done, Done] True : balanceRow rows

(--!) :: [Done] -> [Done] -> [Done]
[] --! [] = []
[] --! _ = error "Error AbacusAPI (--!): Для натурального счёта, вычитается большее число косточек"
a --! [] = a
(Done:fis) --! (Done:sis) = fis --! sis



randomAbacus :: Account (Int, Int)
             -> StdGen
             -> Either NatTheme ExprAbacus
             -> Maybe [RowAbacus]
             -> Either String ([RowAbacus], StdGen)

randomAbacus (Account (minR, maxR)) gen powerTheme mbMode

    -- | isJust mbMode && numAbacus < minR && numAbacus > maxR =   
    --    error $ printf "Error AbacusAPI randomAbacus: Абакус (%d) не входит в диапазон (%d, %d)" numAbacus minR maxR

    | isLeft result = result

    | otherwise = result

    where mode = fromJust mbMode
          logMinus = isRight powerTheme && not (operator . effFromRight $ powerTheme)
          numNewAbacus = (abacusInNum . fst . effFromRight) result
          numAbacus = abacusInNum abacus
          abacus = fromJust mbMode
        
          result = if isJust mbMode
            then randomAbacus' revInvZip invRange RandomVoid 0 firstGen
            else randomAbacus' nullRevInvZip invRange RandomVoid 0 gen
        
          lenMin = length $! minRowsR
          lenMax = length $! maxRowsR
          gapLens = lenMax - lenMin
          invRange = (Min $ replicate gapLens abacus0 ++ reverse minRowsR, Max $ reverse maxRowsR)

          tuple a = (a, gen)
          (tuplePowersAbacus, firstGen) = case powerTheme of
            Left NatMerely -> tuple []
            Left NatBrother -> tuple []
            Left NatFriend -> tuple []
            Right (ExprMerely oper) -> tuple $ flip fmap abacus $ operPowerMerely oper
            Right bro@(ExprBrother _) -> brother abacus gen bro
            Right (ExprFriend oper) -> tuple $ flip fmap abacus $ operPowerFriend oper

          brother [] gen _ = ([], gen)
          brother (r:rs) gen bro =
            let (power, mbCurGen) = operPowerBrother (operator bro) gen r
                curGen = fromMaybe gen mbCurGen
                (futurePowers, futureGen) = brother rs curGen bro
            in (power : futurePowers, futureGen)

          (minRowsR, maxRowsR) = (numInAbacus minR, numInAbacus maxR)

          --powersAbacus = fmap tupleInAbacus tuplePowersAbacus
          zipPowers = mZip3 minRowsR tuplePowersAbacus maxRowsR
          revInvZip = reverse zipPowers

          nullZipPowers = mZip3 minRowsR [] maxRowsR
          nullRevInvZip = reverse nullZipPowers


          randomAbacus' :: [(Maybe RowAbacus, Maybe (Int, Int), Maybe RowAbacus)]
                        -> (Min [RowAbacus], Max [RowAbacus])
                        -> RandomAbacus
                        -> Int
                        -> StdGen
                        -> Either String ([RowAbacus], StdGen)


          randomAbacus' [] (Min invSuncMinR, Max invMaxR) _ _ gen

            | numMin /= numMax = Left "Error AbacusAPI randomAbacus' №2: Получено два разных числа"

            | otherwise = return (newAbacus, gen)

            where suncMinR = reverse invSuncMinR
                  maxR = reverse invMaxR
                  numMin = abacusInNum suncMinR
                  numMax = abacusInNum maxR
                  numNewAbacus = numMin
                  newAbacus = suncMinR


          randomAbacus' ((mbMinRow,mbAbacPower,mbMaxRow):invZs) mainMinMax@(Min invSuncMinR, Max invMaxR) randAb acc gen

            | curNumMinR < minR || curNumMaxR > maxR  =
                let errStr :: String
                    errStr = printf "Error AbacusAPI randomAbacus' №3: Нарушена логика создания абакуса.\nminR:        %d,   maxR:     %d,   powerTheme: %s,  mbMode: %s,\ninvMin:      %s, invMax:  %s, randAb: %s, mbAbacPower: %s, acc: %d,\ncurSuncMinR: %s, curMaxR: %s, curRow: %d,\n\ninvZip (randomAbacus): %s."
                        minR
                        maxR
                        (show powerTheme)
                        (show $ fmap abacusInNum mbMode)

                        (show $ fmap (abacusInNum . return) invSuncMinR)
                        (show $ fmap (abacusInNum . return) invMaxR)
                        (show randAb)
                        (show mbAbacPower)
                        acc

                        (show $ rowInNum <$> curSuncMinR)
                        (show $ rowInNum <$> curMaxR)
                        (abacusInNum [curRow])

                        (show $ if isJust mbMode then revInvZip else nullRevInvZip)
                
                in Left errStr

            | logMinus && (curNumMinR > numAbacus || curNumMaxR > numAbacus) =
                error $ printf "Error AbacusAPI randomAbacus' №1: Абакус (%d) меньше или большн новых промежуточных (curNumMinR: %d, curNumMaxR: %d):\nminR:        %d,   maxR:     %d,   powerTheme: %s,  mbMode: %s,\ninvMin:      %s, invMax:  %s, randAb: %s, mbAbacPower: %s, acc: %d,\ncurSuncMinR: %s, curMaxR: %s, curRow: %d,\n\ninvZip (randomAbacus): %s."
                    numAbacus
                    curNumMinR
                    curNumMaxR

                    minR
                    maxR
                    (show powerTheme)
                    (show $ fmap abacusInNum mbMode)

                    (show $ fmap (abacusInNum . return) invSuncMinR)
                    (show $ fmap (abacusInNum . return) invMaxR)
                    (show randAb)
                    (show mbAbacPower)
                    acc

                    (show $ rowInNum <$> curSuncMinR)
                    (show $ rowInNum <$> curMaxR)
                    (abacusInNum [curRow])

                    (show $ if isJust mbMode then revInvZip else nullRevInvZip)
                
            | otherwise = randomAbacus' newInvZs newRange newRandAb newAcc newGen

            where curMinRow = Min $ fromMaybe abacus0 mbMinRow
                  curPower = fromMaybe (correctPower powerTheme) mbAbacPower
                  curMaxRow = Max $ fromJust mbMaxRow

                  (curRow, newGen) = powerAndScale_InAbacus curPower curMinRow curMaxRow gen

                  ((mCurMin@(Min curSuncMinR), mCurMax@(Max curMaxR)), newRandAb, mbNewZs) =
                    updateCycle randAb mainMinMax curRow acc invZs

                  curNumMinR = abacusInNum $ reverse curSuncMinR
                  curNumMaxR = abacusInNum $ reverse curMaxR

                  newRange = (mCurMin, mCurMax)
                  newAcc = succ acc

                  newInvZs = fromMaybe invZs mbNewZs


correctPower :: Either NatTheme ExprAbacus -> (Int, Int)
correctPower (Left _) = newPower
correctPower (Right (ExprFriend _)) = newPower
correctPower (Right exprAb)
    | operator exprAb = newPower
    | otherwise = (0, 0)


type ForF = (Maybe RowAbacus, Maybe (Int, Int), Maybe RowAbacus)

fResMin :: ForF -> ForF
fResMin = updFstTuple3 (\_ -> Just abacus0)

fResMax :: ForF -> ForF
fResMax = updThrdTuple3 (\_ -> Just abacus9)

fMin :: (t1 -> t2) -> Min t1 -> Min t2
fMin f (Min m) = Min $ f m

fMax :: (t1 -> t2) -> Max t1 -> Max t2
fMax f (Max m) = Max $ f m


{-| # Ошибка найдена:
  | Числа проверяются только на равенство
  | 
  | Для тестов:
  | => mainMax = (Min [abacus1, abacus0, abacus0], Max [abacus8, abacus8, abacus5])
  | => invZs = mZip3 [abacus1, abacus0, abacus0] [(3,1), (3,1), (0,1)] [abacus8, abacus8, abacus5]
  | => curInvZs = tail invZs
  | => upd = updateCycle RandomVoid mainMax abacus8 0 curInvZs
  | => newUpd = updFstTuple3 (updFstTuple2 (fMin (abacusInNum . reverse))) upd
  | => newUpd2 = updFstTuple3 (updSndTuple2 (fMax (abacusInNum . reverse))) newUpd
  |
  | => forF = fromJust $ thd3 newUpd
  | => forFFsts = fmap (rowInNum . fromJust . fst3) forF
  | => forFThrds = fmap (rowInNum . fromJust . thd3) forF
  | => zip forFFsts forFThrds
  |-}
updateCycle :: RandomAbacus
            -> (Min [RowAbacus], Max [RowAbacus])
            -> RowAbacus
            -> Int
            -> [ForF]
            -> ((Min [RowAbacus], Max [RowAbacus]), RandomAbacus, Maybe [ForF])

updateCycle randAb
            (Min invSuncMinR, Max invMaxR)
            curRow
            acc
            invZs

    = case randAb of

    RandomVoid -> case (logMin, logMax) of
        (True, True)    ->  ((min_STAND, max_STAND), RandomMinMax, upd [])
        (True, False)   ->  ((min_STAND, max_NEU), RandomMin, upd [fResMax])
        (False, True)   ->  ((min_NEU, max_STAND), RandomMax, upd [fResMin])
        (False, False)  ->  ((min_NEU, max_NEU), RandomNeutral, upd [fResMin, fResMax])

    RandomMinMax -> case (logMin, logMax) of
        (True, True)    ->  ((min_STAND, max_STAND), RandomMinMax, upd [])
        (True, False)   ->  ((min_STAND, max_NEU), RandomMin, upd [fResMax])
        (False, True)   ->  ((min_NEU, max_STAND), RandomMax, upd [fResMin])
        (False, False)  ->  ((min_NEU, max_NEU), RandomNeutral, upd [fResMin, fResMax])

    RandomMin -> if logMin
        then                ((min_STAND, max_NEU), RandomMin, upd [fResMax])
        else                ((min_NEU, max_NEU), RandomNeutral, upd [fResMin, fResMax])

    RandomMax -> if logMax
        then                ((min_NEU, max_STAND), RandomMax, upd [fResMin])
        else                ((min_NEU, max_NEU), RandomNeutral, upd [fResMin, fResMax])

    RandomNeutral       ->  ((min_NEU, max_NEU), RandomNeutral, upd [fResMin, fResMax])

    where cupAcc = succ acc

          logMin = invSuncMinR !! acc == curRow
          logMax = invMaxR !! acc == curRow

          min_STAND = Min invSuncMinR
          max_STAND = Max invMaxR

          min_NEU = func_NEU Min invSuncMinR abacus0
          max_NEU = func_NEU Max invMaxR abacus9

          func_NEU returnNum invL curAbacus =
            let headScale = flip take invL $ pred cupAcc
                tailInv = drop cupAcc invL

            in returnNum $ headScale ++ return curRow ++ replicate (length tailInv) curAbacus


          upd :: [ForF -> ForF] -> Maybe [ForF]

          upd [] = Nothing
          upd lFunc = return $ upd' lFunc invZs
                    
            where upd' [] l = l
                  upd' (f:fs) l = upd' fs $ fmap f l


{-| Протестил на:
  | => curAbacus gen = powerAndScale_InAbacus (4,0) (Min abacus4) (Max abacus6) gen
  | => lambda gen _ = let (ab,ng) = curAbacus gen in do {print $ rowInNum ab; return ng}
  | => import Control.Monad
  | => foldM_ lambda gen (replicate 5 ())
  |-}
powerAndScale_InAbacus :: (Int, Int) -> Min RowAbacus -> Max RowAbacus -> StdGen -> (RowAbacus, StdGen)
powerAndScale_InAbacus power (Min minA) (Max maxA) gen
    | minA > maxA = error "Error AbacusAPI powerAndScale_InAbacus: Меньшее больше большего числа"
    | minA > powerA = error "Error AbacusAPI powerAndScale_InAbacus: Минимальное число не входит в диапазон"

    | (minA == powerA) && (powerA == maxA) = tuple powerA
    
    | otherwise = finalRowGen

    where tuple a = (a, gen)
          powerA = tupleInAbacus power

          (logOption, optionGen) = flip randomR gen ((0, 1) :: (Int, Int))
          groundPowerA = powerA {upper = False}

          finalRowGen = case (upper minA, upper powerA, upper maxA) of
            (True, _, False) -> error "Error AbacusAPI powerAndScale_InAbacus: Минимальное число больше максимального"

            (False, True, False) -> flip genRow gen $ equality $ groundPowerA
            (False, False, True) -> flip genRow gen $ inequality_UpTo5 powerA
            (False, True, True) -> if logOption == 1
                then genRow inequality_UpTo10 optionGen
                else flip genRow optionGen $ inequality_UpTo5 groundPowerA
            _ -> flip genRow gen $ equality powerA
        
          equality powerA = if lower maxA < lower powerA
            then (minA, maxA)
            else (minA, powerVSmax)

            where powerVSmax = if upper powerA && not (upper maxA)
                    then maxA {lower = lower powerA}
                    else powerA
        
          inequality_UpTo5 powerA = (minA, powerA)
            
          inequality_UpTo10 = if lower maxA < lower powerA
            then (abacus5, maxA)
            else (abacus5, powerA)
        {-
          (option, firstGen) = flip randomR gen $ (0, 1) :: (Int, Int)
          doubles = if option == 0 then (1, 0) else (0, 1)

          (floorLower, floorUpper) = if bool
            then case nums of
                (_, 0) -> (1, 0)
                (0, _) -> (0, 1)
                _ -> doubles
            else (0, 0)
        -}
          funcLower low = replicate low Done

          genRow (finMin, finMax) gen =
            let (randomLower, firstGen) = randomR (length $ lower finMin, length $ lower finMax) gen
            in (finMin {lower = funcLower randomLower}, firstGen)

--powerInAbacus (numLower, numUpper) = RowAbacus (take numLower $ repeat Done) (if numUpper == 0 then False else True)


-- | Сначала идут единицы
createRows :: Int -> [Int]
createRows num
    | num == 0 = [0]
    | otherwise = createRows' $ accRows 1 9

    where accRows :: Int -> Int -> [(Int, Int)]
          accRows powerRowSimula numSimula
            | num <= numSimula = [tupleRow]
            | otherwise = tupleRow : accRows newRow newNumSimula

            where tupleRow = (newRow, powerRowSimula)
                  newRow = powerRowSimula * 10
                  newNumSimula = exponent numSimula

                  exponent sim = sim * 10 + sim

          createRows' :: [(Int, Int)] -> [Int]
          createRows' [] = []
          createRows' ((modRow, divRow):rows)
            = num `mod` modRow `div` divRow : createRows' rows


createAbacus :: [Int] -> [RowAbacus]
createAbacus [] = []
createAbacus (row:rows)
    | row < 0 = error "Error AbacusAPI createAbacus: Получено отрицательное число разряда для абакуса"
    | row >= 5 = RowAbacus (funcLower balanceRower) True : createAbacus rows
    | otherwise = RowAbacus (funcLower row) False : createAbacus rows

    where balanceRower = row - 5
          funcLower l = replicate l Done


numInAbacus :: Int -> [RowAbacus]
numInAbacus num
    | num >= 0 = abacus
    | otherwise = error "Error AbacusAPI numInAbacus: На преобразование в абакус дано отрицательное число"

    where abacus = createAbacus listNumber
          listNumber = createRows num


abacusInNum :: [RowAbacus] -> Int
abacusInNum abacus = internalNum $ zip (iterate (*10) 1) abacus

    where internalNum [] = 0
          internalNum ((row, RowAbacus lower upper):abacusis) = let numUpper = if upper then 5 * row else 0
            in length lower * row + numUpper + internalNum abacusis


newMerelyPowerPlus :: RowAbacus -> (Int, Int)
newMerelyPowerPlus (RowAbacus lower upper)
    | upper = (powerLower, 0)
    | otherwise = (powerLower, 1)

    where powerLower = 4 - length lower

newBrotherPowerPlus :: StdGen -> RowAbacus -> ((Int, Int), Maybe StdGen)
newBrotherPowerPlus gen (RowAbacus lower upper)
    | upper = shell (powerLower, 0)
    | otherwise = if log
        then cont (powerLower, 1)
        else cont (4, 0)

    where powerLower = 4 - length lower
          cont a = (a, return finalGen)
          shell a = (a, Nothing)
          (logNum, finalGen) = randomR ((0, 1) :: (Int, Int)) gen
          log = logNum /= 0

-- Потом допишу «Друг + Брат»
newFriendPowerPlus :: RowAbacus -> (Int, Int)
newFriendPowerPlus (RowAbacus lower upper) = (4, 1)


newMerelyPowerMinus :: RowAbacus -> (Int, Int)
newMerelyPowerMinus (RowAbacus lower upper)
    | upper = (powerLower, 1)
    | otherwise = (powerLower, 0)

    where powerLower = length lower

newBrotherPowerMinus :: StdGen -> RowAbacus -> ((Int, Int), Maybe StdGen)
newBrotherPowerMinus gen (RowAbacus lower upper)
    | upper = shell (powerLower, 1)
    | otherwise = shell (powerLower, 0)
    
    where powerLower = length lower
          shell a = (a, Nothing)

-- Потом допишу «Друг + Брат»
newFriendPowerMinus :: RowAbacus -> (Int, Int)
newFriendPowerMinus (RowAbacus lower upper) = (4, 1)

newPower :: (Int, Int)
newPower = (4, 1)


operPowerMerely :: Bool -> (RowAbacus -> (Int, Int))
operPowerMerely log
    | log = newMerelyPowerPlus
    | otherwise = newMerelyPowerMinus

operPowerBrother :: Bool -> (StdGen -> RowAbacus -> ((Int, Int), Maybe StdGen))
operPowerBrother log
    | log = newBrotherPowerPlus
    | otherwise = newBrotherPowerMinus

operPowerFriend :: Bool -> (RowAbacus -> (Int, Int))
operPowerFriend log
    | log = newFriendPowerPlus
    | otherwise = newFriendPowerMinus


abacus0 :: RowAbacus
abacus1 :: RowAbacus
abacus2 :: RowAbacus
abacus3 :: RowAbacus
abacus4 :: RowAbacus
abacus5 :: RowAbacus
abacus6 :: RowAbacus
abacus7 :: RowAbacus
abacus8 :: RowAbacus
abacus9 :: RowAbacus

abacus0 = RowAbacus [] False
abacus1 = RowAbacus [Done] False
abacus2 = RowAbacus [Done, Done] False
abacus3 = RowAbacus [Done, Done, Done] False
abacus4 = RowAbacus [Done, Done, Done, Done] False
abacus5 = RowAbacus [] True
abacus6 = RowAbacus [Done] True
abacus7 = RowAbacus [Done, Done] True
abacus8 = RowAbacus [Done, Done, Done] True
abacus9 = RowAbacus [Done, Done, Done, Done] True


clearVoidRows :: [RowAbacus] -> [RowAbacus]
clearVoidRows rows = if null result
    then [RowAbacus [] False]
    else result

    where result = snd $ nestedClear rows
        
          nestedClear :: [RowAbacus] -> (Bool, [RowAbacus])
          nestedClear [] = (True, [])

          nestedClear (row@(RowAbacus [] False):rows) =
            let (futureLogVoid, futureRows) = nestedClear rows
            in if futureLogVoid
                then (True, [])
                else (False, row : futureRows)

          nestedClear (row:rows) =
            let (_, futureRows) = nestedClear rows
            in (False, row : futureRows)


overlayAbacus :: RowAbacus -> RowAbacus -> RowAbacus
overlayAbacus firstRow secondRow = RowAbacus curLower curUpper

    where curLower = take 4 (lower firstRow ++ lower secondRow)
          curUpper = upper firstRow || upper secondRow


countRange :: Int -> Amount (Int, Int) -> Account (Int, Int)
countRange numOp (Amount (dirtMinR, dirtMaxR)) =
    let (minR, maxR) = (abs dirtMinR, abs dirtMaxR)
        evenMinR = pred minR
    in Account (if evenMinR <= 0 then numOp else 10 ^ evenMinR, pred $ 10 ^ maxR)


tupleInNum :: (Int, Int) -> Int
tupleInNum (lower, upper) = 5 * upper + lower


tupleInAbacus :: (Int, Int) -> RowAbacus
tupleInAbacus (lower, upper) = RowAbacus (replicate lower Done) (upper /= 0)


abacusInTuple :: RowAbacus -> (Int, Int)
abacusInTuple (RowAbacus lower upper) = (curLower, curUpper)

    where curLower = length lower
          curUpper = if upper then 1 else 0


rowInNum :: RowAbacus -> Int
rowInNum (RowAbacus lower upper) = curLower + 5 * curUpper

    where curLower = length lower
          curUpper = if upper then 1 else 0
