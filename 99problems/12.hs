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

-- New stuff

decodeModified :: [EncodingAtom a] -> [a]
decodeModified xs = foldr func [] xs where
  func (Multiple n v) xs = concat [(replicate n v), xs]
  func (Single v) xs = v : xs
