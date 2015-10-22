{-
  (**) Eliminate consecutive duplicates of list elements.

  If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

  Example:

  > compress "aaaabccaadeeee"
  "abcade"
-}

compress :: Eq a => [a] -> [a]
compress (x:y:[]) | x == y = x:[]
                  | otherwise = x:y:[]
compress (x:y:xs) | x == y = compress $ x:xs
                  | otherwise = x : (compress $ y:xs)
compress a = a
