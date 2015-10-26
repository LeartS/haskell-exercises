{-
  (*) Split a list into two parts; the length of the first part is given.
  -----------------------------------------------------------------------

  Do not use any predefined predicates.

  Example in Haskell:

  *Main> split "abcdefghik" 3
  ("abc", "defghik")
-}


-- Doing it this way supports infinite lists!
-- Try `split [1..] 10` or, better `fst $ split [1..] 10`
split :: [a] -> Int -> ([a], [a])
split (x:xs) n = split' (n-1) x xs where
  split' 0 x (y:yx) = (x:[], y:yx)
  split' n x (y:ys) = (x : (fst res), snd res) where
    res = split' (n-1) y ys
