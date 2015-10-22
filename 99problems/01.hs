-- (*) Find the last element of a list.
-- Example in Haskell:
-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'


myLast sequence = drop ((length sequence) - 1) sequence
