mydrop n xs = if n <= 0 || null(xs)
                 then xs
                      else
                mydrop (n-1) (tail xs)



             then xs
             else 
             mydrop (n-1) (tail xs)

data MyList a = Cons a (MyList a)
| None
deriving (Show)

fromList x:xs = Cons x (fromList xs)
fromList [] = None


--main = mydrop 3 "vishal"

