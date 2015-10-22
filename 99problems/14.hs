{-
  (*) Duplicate the elements of a list.
  -------------------------------------

  (A A B B C C C C D D)
  Example in Haskell:

  > dupli [1, 2, 3]
  [1,1,2,2,3,3]
-}


dupli :: [a] -> [a]
dupli = foldr (\x xs -> x:x:xs) []
