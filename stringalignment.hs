{-

1) General understanding of the problem

An application of the string alignment problem to computer science is the fact that the maximal common subsequence problem (MCS) is a special case of the string alignment problem.
Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?

Answer:
If we had access to an algorithm, then we could use it to solve the MCS by setting the score for a mismatches and spacees to zero. This would result in only the matches retrieving a score.

-}

module StringAlignment where

scoreMatch    = 0
scoreMismatch = (-1)
scoreSpace    = (-1)
string1 = "writers"
string2 = "vintner"

-- 2.a)
-- http://en.wikipedia.org/wiki/Smith-Waterman_algorithm
-- Will calculate the similarity between two string sequences using a gap-scoring scheme.
-- Optimized version
similarityScore :: String -> String -> Int
similarityScore xs ys = getScore (length xs) (length ys)
  where getScore :: Int -> Int -> Int
        getScore i j = scoreTable !! i !! j
        
        scoreTable :: [[Int]]
        scoreTable = [[scoreEntry i j | j <- [0..] ] | i <- [0..]]
        
        scoreEntry :: Int -> Int -> Int
        scoreEntry 0 0 = 0
        scoreEntry i 0 = scoreSpace * i -- w(a, -), took forever to realize :(
        scoreEntry 0 j = scoreSpace * j -- w(-, b), -"-
        scoreEntry i j = maximum [ valueNW + (scorer x   y)
                                 , valueW  + (scorer x '-')
                                 , valueN  + (scorer '-' y)
                                 ]
          where valueNW = (getScore (i-1) (j-1))
                valueN  = (getScore (  i) (j-1))
                valueW  = (getScore (i-1) (  j))
                x = xs !! (i-1)
                y = ys !! (j-1)

-- Calculates the score for a column in the two sequences, a column is represented by two separate Chars.
scorer :: Char -> Char -> Int
scorer  _ '-' = scoreSpace
scorer '-' _  = scoreSpace
scorer  x  y 
  | x == y    = scoreMatch
  | otherwise = scoreMismatch
                        
-- 2.b)
-- `attachHeads` takes two arguments of type a, and conses them onto each list, in 
-- the list of tuples of lists. 
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]

-- 2.c) Maximizes a list by the specified function
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = map fst $ filter ((>= (maximum $ transformed)) . snd ) $ zip xs $ transformed
  where transformed = map valueFcn xs
        
-- 2.d) Generates the optimal alignments
type AlignmentType = (String, String)

optAlignmentsUnoptimized :: String -> String -> [AlignmentType]
optAlignmentsUnoptimized xs ys  = map (\(x,y) -> (reverse x, reverse y)) $ getAlignment (length xs) (length ys)
  where getAlignment :: Int -> Int -> [AlignmentType]
        getAlignment i j = alignmentTable !! i !! j

        alignmentTable :: [[[AlignmentType]]]
        alignmentTable = [[alignmentEntry i j | j <- [0..]] | i <- [0..]]
        
        alignmentEntry :: Int -> Int -> [AlignmentType]
        alignmentEntry 0 0 = [([], [])]
        alignmentEntry i 0 = attachHeads (xs !! (i-1)) (        '-') $ getAlignment (i-1) 0
        alignmentEntry 0 j = attachHeads (        '-') (ys !! (j-1)) $ getAlignment 0 (j-1)
        alignmentEntry i j 
          | (xs !! (i-1)) == (ys !! (j-1)) = attachHeads (xs !! (i-1)) (ys !! (j-1)) $ getAlignment (i-1) (j-1)
          -- maximaBy similarity will reduce the generated list
          | otherwise = maximaBy similarity $ concat [attachHeads (xs !! (i-1)) (ys !! (j-1)) $ getAlignment (i-1) (j-1)
                                                     ,attachHeads (        '-') (ys !! (j-1)) $ getAlignment     i (j-1)
                                                     ,attachHeads (xs !! (i-1)) (        '-') $ getAlignment (i-1)     j
                                                     ]
similarity :: AlignmentType -> Int
similarity (xs,ys) = sum $ zipWith scorer xs ys

-- 2.e) Outputs the optimal alignments
outputAlignments xs ys = do
  putStrLn $ "There are " ++ (show (length result)) ++ " optimal alignments:\n"
  mapM_ (\(x,y) -> mapM putStrLn [x,y, ""] ) result
    where result = optAlignments xs ys 
                                            
                        
optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys  = map (\(x,y) -> (reverse x, reverse y)) $ snd $ getAlignment (length xs) (length ys)
  where getAlignment :: Int -> Int -> (Int, [AlignmentType])
        getAlignment i j = alignmentTable !! i !! j

        alignmentTable :: [[(Int, [AlignmentType])]]
        alignmentTable = [[alignmentEntry i j | j <- [0..]] | i <- [0..]]
        
        alignmentEntry :: Int -> Int -> (Int, [AlignmentType])
        alignmentEntry 0 0 = (0, [([], [])])
        
        alignmentEntry i 0 = (scoreSpace + v, (attachHeads x '-' w))
          where (v, w) = getAlignment (i-1) 0
                x = xs !! (i-1)
                
        alignmentEntry 0 j = (scoreSpace + v, (attachHeads '-' y w))
          where (v, w) = getAlignment 0 (j-1)
                y = ys !! (j-1)
                
        alignmentEntry i j 
          | x == y = let result = getAlignment (i-1) (j-1) in
                      (scoreMatch + (fst result), attachHeads x y $ snd result)
                      
          | otherwise =  (fst $ head values, concatMap snd values)                                      
                         
                        where (vNW,wNW) = getAlignment (i-1) (j-1)
                              (vN,wN)   = getAlignment     i (j-1)
                              (vW,wW)   = getAlignment (i-1)     j
                              
                              -- maximaBy fst will reduce the generated list by maximizing on the first 
                              -- element in each tuple of the list
                              values = maximaBy fst [(scoreMismatch + vNW, (attachHeads   x   y  wNW))
                                                    ,(scoreMismatch + vN,  (attachHeads '-'   y   wN))
                                                    ,(scoreMismatch + vW,  (attachHeads   x '-'   wW))
                                                    ]
                              x = (xs !! (i-1))
                              y = (ys !! (j-1))
                        



