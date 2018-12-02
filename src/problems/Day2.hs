module Main where

import           Data.List                                ( sort
                                                          , foldl'
                                                          )
import           Data.Map.Lazy                            ( Map )
import qualified Data.Map.Lazy                 as Map

type ID = String
type Checksum = Int

countLetters :: ID -> [Int]
countLetters id = sort . Map.elems $ foldl'
  (\count k -> Map.insertWith (+) k 1 count)
  Map.empty
  id

checkSumReqs :: ID -> (Bool, Bool)
checkSumReqs id = (hasExactly2, hasExactly3)
 where
  lessThan4   = takeWhile (< 4) $ countLetters id
  lessThan3   = takeWhile (< 3) lessThan4
  hasExactly3 = not (null lessThan4) && (last lessThan4 == 3)
  hasExactly2 = not (null lessThan3) && (last lessThan3 == 2)

accumTrue :: Int -> Bool -> Int
accumTrue x cond = if cond then x + 1 else x

-- worse function name ever
-- probably should have just used a Bifunctor.
doubleUp :: (b -> a -> b) -> ((b, b) -> (a, a) -> (b, b))
doubleUp f (x, y) (i, j) = (f x i, f y j)

part1 :: [ID] -> Checksum
part1 ids =
  uncurry (*) $ foldl' (doubleUp accumTrue) (0, 0) $ checkSumReqs <$> ids

main :: IO ()
main = do
  input <- readFile "inputs/day2.txt"
  let solution1 = part1 (lines input)
  print solution1

