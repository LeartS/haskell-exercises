{-
  (*) Reverse a list.

  Prelude> myReverse "A man, a plan, a canal, panama!"
  "!amanap ,lanac a ,nalp a ,nam A"

  Prelude> myReverse [1,2,3,4]
  [4,3,2,1]
-}

import Data.List

myReverse xs = foldl' (flip (:)) [] xs
