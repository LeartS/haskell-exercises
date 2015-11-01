{-
  (*) Remove the K'th element from a list.

  Example in Haskell:

  *Main> removeAt 2 "abcd"
  "acd"
-}

-- Version using list comprehension
removeAt :: Int -> [a] -> [a]
removeAt n xs = [x | (x, y) <- zip xs [1 .. length xs], y /= n]


-- Version using prelude functions that works on infinite lists
removeAt2 :: Int -> [a] -> [a]
removeAt2 n xs = take (n-1) xs ++ drop n xs
