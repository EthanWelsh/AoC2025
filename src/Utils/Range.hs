module Utils.Range
  ( Range,
    Ranges,
    member,
    overlap,
    intersect,
    mergeRanges,
    rangeSize,
    memberOfRanges,
  )
where

import Data.List (sortOn)

type Range = (Int, Int)

type Ranges = [Range]

-- | Checks if a number is within any of the given ranges.
memberOfRanges :: Ranges -> Int -> Bool
memberOfRanges ranges n = any (member n) ranges

-- | Checks if a number is within a given range (inclusive).
member :: Int -> Range -> Bool
member x (start, end) = x >= start && x <= end

-- | Checks if two ranges overlap.
overlap :: Range -> Range -> Bool
overlap (s1, e1) (s2, e2) = not (e1 < s2 || e2 < s1)

-- | Finds the intersection of two ranges.
intersect :: Range -> Range -> Maybe Range
intersect (s1, e1) (s2, e2) =
  let start = max s1 s2
      end = min e1 e2
   in if start <= end then Just (start, end) else Nothing

-- | Merges a list of overlapping ranges into a minimal set of non-overlapping ranges.
mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges ranges = foldl merge [] (sortOn fst ranges)
  where
    merge [] current = [current]
    merge acc@((lastStart, lastEnd) : rest) currentRange@(currentStart, currentEnd)
      | currentStart <= lastEnd + 1 = (lastStart, max lastEnd currentEnd) : rest
      | otherwise = currentRange : acc

-- | Calculates the size of a range (inclusive).
rangeSize :: Range -> Int
rangeSize (start, end) = end - start + 1
