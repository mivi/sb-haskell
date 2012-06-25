-- Based on Fig 5 on http://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1/
-- Lets try to use a function that maps String to Integer (length) and lift it for list of list of list of String

import Data.String

-- lift length

-- apply on list of String
-- :type (map length) 
--   [[a]] -> [Int]

-- apply on list of list of String
-- *Main> :type (map (map length))
-- (map (map length)) :: [[[a]]] -> [[Int]]

f = map (map length)

main = do
  let a=[[["Jim", "harry"]], [["Jennifer", "Jennifer bro", "Jennifer sis"]]]
  print $ map f a