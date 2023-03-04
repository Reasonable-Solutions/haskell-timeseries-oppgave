{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (assert)
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Text.Pretty.Simple (pPrint)

data TimeRange = TimeRange
  { start :: Int,
    end :: Int
  }
  deriving (Show, Read, Eq)

data Status a = Sick a | Well a deriving (Show, Functor)

-- ranges are inclusive
duration :: TimeRange -> (Int, Int, Int)
duration (TimeRange beginning end) = (beginning, end, end - beginning + 1)

sickWell :: (TimeRange, TimeRange) -> [Status TimeRange]
sickWell ((TimeRange a b), (TimeRange c d)) = [Sick $ TimeRange a b, Well $ TimeRange (b + 1) (c - 1), Sick $ TimeRange c d]

periodsAndPauses :: [Status (Int, Int, Int)] -> (Int, [Int])
periodsAndPauses ps = (periodLength ps, mapMaybe wellMoreThanSixteenDays ps)

valids :: (Int, [Int]) -> Bool
valids (length, pauses)
  | (length > 42) = not $ any (> 16) pauses
  | (length <= 42) = False

main :: IO ()
main = do
  cases <- traverse readRange ["data1.txt", "data2.txt", "data3.txt", "data4.txt"]
  let res = cases <&> (periodsAndPauses . (fmap . fmap $ duration) . concat . (fmap sickWell) . zippy)

  pPrint $ valids <$> res
  print $ assert (fmap valids res == [True, True, False, False]) "cases work"
  where
    readRange f = read @[TimeRange] <$> readFile f

zippy :: [TimeRange] -> [(TimeRange, TimeRange)]
zippy ranges = zip ranges $ tail ranges

wellMoreThanSixteenDays :: (Ord a, Num a) => Status (a, a, a) -> Maybe a
wellMoreThanSixteenDays (Well (_, _, a))
  | a > 16 = Just a
  | otherwise = Nothing
wellMoreThanSixteenDays (Sick _) = Nothing

periodLength :: [Status (Int, Int, Int)] -> Int
periodLength ranges = maximum (unEnd <$> ranges) - minimum (unStart <$> ranges) + 1

unStart (Sick (x, _, _)) = x
unStart (Well (x, _, _)) = x

unEnd (Sick (_, x, _)) = x
unEnd (Well (_, x, _)) = x
