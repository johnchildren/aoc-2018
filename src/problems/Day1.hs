module Main where

import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set

type Token = String
type Direction = Integer -> Integer -> Integer
type Magnitude = Integer

type FreqChange = (Direction, Magnitude)

-- naughty partial function
readChange :: Token -> FreqChange
readChange ('+' : xs) = ((+), read xs)
readChange ('-' : xs) = ((-), read xs)
readChange t          = error ("invalid token " ++ t)

nextMagnitude :: Magnitude -> [FreqChange] -> (Magnitude, [FreqChange])
nextMagnitude x []            = (x, [])
nextMagnitude x ((f, y) : fs) = (f x y, fs)

calculateChange :: Magnitude -> [FreqChange] -> Magnitude
calculateChange x []    = x
calculateChange x freqs = uncurry calculateChange $ nextMagnitude x freqs

part1 :: [Token] -> Magnitude
part1 tokens = calculateChange 0 $ readChange <$> tokens

findDuplicate
  :: (Magnitude, Set Magnitude) -> [FreqChange] -> (Magnitude, Set Magnitude)
findDuplicate info [] = info -- shouldn't happen, should cycle
findDuplicate (x, seen) freqs =
  let (next, fs) = nextMagnitude x freqs
  in  if Set.member next seen
        then (next, seen)
        else findDuplicate (next, Set.insert next seen) fs

part2 :: [Token] -> Magnitude
part2 tokens = fst $ findDuplicate (0, Set.singleton 0) $ cycle $ readChange <$> tokens

main :: IO ()
main = do
  input <- readFile "inputs/day1.txt"
  let solution1 = part1 (lines input)
  print solution1
  let solution2 = part2 (lines input)
  print solution2
