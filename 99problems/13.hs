{-
  (**) Decode a run-length encoded list.
  --------------------------------------

  Given a run-length code list generated as specified in problem 11.
  Construct its uncompressed version.

  Example in Haskell:

  P12> decodeModified
         [Multiple 4 'a',Single 'b',Multiple 2 'c',
          Multiple 2 'a',Single 'd',Multiple 4 'e']
  "aaaabccaadeeee"
-}

-- Taken from previous exercise
data EncodingAtom a = Multiple Int a | Single a
                    deriving (Show, Eq)

encodeDirect :: Eq a => [a] -> [EncodingAtom a]
encodeDirect [] = []
encodeDirect all@(x:xs)
  | n == 1 = Single value : (encodeDirect $ snd pieces)
  | otherwise = Multiple n value : (encodeDirect $ snd pieces)
  where
    pieces = span (== x) all
    n = length $ fst pieces
    value = head $ fst pieces
