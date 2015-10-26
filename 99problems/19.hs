{-
  (**) Rotate a list N places to the left.
  ----------------------------------------

  Hint: Use the predefined functions length and (++).

  Examples in Haskell:

  *Main> rotate ['a','b','c','d','e','f','g','h'] 3
  "defghabc"

  *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
  "ghabcdef"
-}

rotate :: [a] -> Int -> [a]
rotate xs n | (n > length xs) || (n < 0) = rotate xs (n `mod` length xs)
            | otherwise = (drop n xs) ++ (take n xs)
