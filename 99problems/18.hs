{-
  (**) Extract a slice from a list.
  ---------------------------------

  Given two indices, i and k, the slice is the list containing the elements
  between the i'th and k'th element of the original list (both limits included).
  Start counting the elements with 1.

  Example in Haskell:

  *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
  "cdefg"
-}


-- Similar to previous exercise, this also supports infinite lists
slice :: [a] -> Int -> Int -> [a]
slice (x:xs) n1 n2 = slice' n1 n2 x xs where
  slice' n1 n2 x  (y:ys) | n1 > n2 = []
                         | n2 == 1 = x:[]
                         | n1 == 1 = x : (slice' 1 (n2-1) y ys)
                         | otherwise = slice' (n1-1) (n2-1) y ys
  slice' 1  _  x  [] = x:[]
  slice' _  _  x  [] = []
