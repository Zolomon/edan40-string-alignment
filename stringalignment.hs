{-

1) General understanding of the problem

An application of the string alignment problem to computer science is the fact that the maximal common subsequence problem (MCS) is a special case of the string alignment problem.
Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?

Answer:
If we had access to an algorithm, then we could use it to solve the MCS by setting the score for a mismatches and spacees to zero. This would result in only the matches retrieving a score.

-}

module StringAlignment where

scoreMatch    = 1
scoreMismatch = (-1)
scoreSpace    = (-2) 

similarityScore :: String -> String -> Int
similarityScore top bot = sum $ zipWith scorer top bot 
  where scorer :: Char -> Char -> Int
        scorer  _ '-' = scoreSpace
        scorer '-' _  = scoreSpace
        scorer  x  y 
          | x == y    = scoreMatch
          | otherwise = scoreMismatch
                        
-- 2.b)
-- `attachHeads` takes two arguments of type a, and conses them onto each list, in 
-- the list of tuples of lists. That is, executing the below in GHCi:

-- Prelude> attachHeads 0 (-1) [([1],[2]),([3],[4])] 
-- [([0,1],[-1,2]),([0,3],[-1,4])].

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = map fst $ filter ((>= (maximum $ transformed)) . snd ) $ zip xs $ transformed
  where transformed = map valueFcn xs
        
type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 