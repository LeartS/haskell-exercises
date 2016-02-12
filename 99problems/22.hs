{-
  Create a list containing all integers within a given range.

  Example in Haskell:

  Prelude> range 4 9
  [4,5,6,7,8,9]

-}

range start end | start > end = error "Start cannot be less than end"
                | start == end = end : []
                | otherwise = start : range (succ start) end
