# 1. General understanding of the problem

An application of the string alignment problem to computer science is the fact that the maximal common subsequence problem (MCS) is a special case of the string alignment problem.
Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?

## Answer
If we had access to an algorithm, then we could use it to solve the MCS by setting the score for a mismatches and spacees to zero. This would result in only the matches retreiving 

# 2. Haskell programming

b) `attachHeads` takes two 'a's and conses them onto each list, in the list of tuples of lists. That is, attachHeads 0 (-1) [([1],[2]),([3],[4])] will result in [([0,1],[-1,2]),([0,3],[-1,4])].

