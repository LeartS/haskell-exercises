{-
  (**) Replicate the elements of a list a given number of times.
  --------------------------------------------------------------

  Example in Haskell:

  > repli "abc" 3
  "aaabbbccc"
-}

-- Using replicate or (take n) . repeat is too easy!
repli :: [a] -> Int -> [a]
repli xs n = foldr (func n) [] xs where
  func 0 v xs = xs
  func n v xs = v : func (n - 1) v xs
