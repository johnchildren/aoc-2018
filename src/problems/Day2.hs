module Main where

import           Data.List                                ( sort
                                                          , foldl'
                                                          )
import           Data.Map.Lazy                            ( Map )
import qualified Data.Map.Lazy                 as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set

type ID = String
type Checksum = Int

makeCount :: ID -> Map Char Int
makeCount = foldl' (\count k -> Map.insertWith (+) k 1 count) Map.empty

possibleFrequencies :: ID -> Set Int
possibleFrequencies = Set.fromList . Map.elems . makeCount

checkSumReqs :: ID -> (Bool, Bool)
checkSumReqs id = (2 `Set.member` freqs, 3 `Set.member` freqs)
 where
   freqs = possibleFrequencies id

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

