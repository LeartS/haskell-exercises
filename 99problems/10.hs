{-
    (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length
    encoding data compression method.
    Consecutive duplicates of elements are encoded as lists (N E) where N is
    the number of duplicates of the element E.

    Example:

    encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}


-- Taken and improved from previous exercise
pack :: Eq a => [a] -> [[a]]
pack = foldr func [[]] where
  func x all@(y:ys) | y == [] = [[x]]
                    | x == (head y) = (x:y):ys
                    | otherwise = [x]:all

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\all@(y:ys) -> (length all, y)) $ pack xs
