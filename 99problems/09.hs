{-
  (**) Pack consecutive duplicates of list elements into sublists.
  If a list contains repeated elements they should be placed in separate sublists.

  Example in Haskell:

  *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
               'a', 'd', 'e', 'e', 'e', 'e']
  ["aaaa","b","cc","aa","d","eeee"]
-}

pack' :: Eq a => a -> [[a]] -> [[a]]
pack' x ([]:ls) = [x] : ls
pack' x y@(l:ls) | x == head l = (x : l) : ls
                 | otherwise = [x] : y

pack :: Eq a => [a] -> [[a]]
pack = foldr pack' [[]]
