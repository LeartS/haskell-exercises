{-
  (**) Drop every N'th element from a list.
  -----------------------------------------

  Example in Haskell:

  *Main> dropEvery "abcdefghik" 3
  "abdeghk"
-}


dropEvery' :: Int -> [a] -> Int -> [a]
dropEvery' _ []     _ = []
dropEvery' n (x:xs) d | remainder == 0 = dropEvery' (remainder + 1) xs d
                      | otherwise = x : dropEvery' (remainder + 1) xs d
  where remainder = n `mod` d

dropEvery :: [a] -> Int -> [a]
dropEvery = dropEvery' 1
