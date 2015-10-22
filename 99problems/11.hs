{-
  Modified run-length encoding.
  -----------------------------

  Modify the result of problem 10 in such a way that if an element has no
  duplicates it is simply copied into the result list.
  Only elements with duplicates are transferred as (N E) lists.

  Example in Haskell:

  P11> encodeModified "aaaabccaadeeee"
  [Multiple 4 'a',Single 'b',Multiple 2 'c',
   Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

-- Taken from previous exercise
pack :: Eq a => [a] -> [[a]]
pack = foldr func [[]] where
  func x all@(y:ys) | y == [] = [[x]]
                    | x == (head y) = (x:y):ys
                    | otherwise = [x]:all

-- New stuff
data EncodingAtom a = Multiple Int a | Single a
                    deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [EncodingAtom a]
encodeModified xs = map func $ pack xs where
  func all@(y:_) | length all == 1 = Single y
                 | otherwise = Multiple (length all) y
