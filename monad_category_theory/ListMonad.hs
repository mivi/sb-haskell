-- import Control.Monad

-- instance Monad [] where
--   return x = [x]
--   x >>= f = concat (map f x)
--   x >> f = concat (map (\_ -> f) x)
--   fail _ = []

cartesianMonad xs ys = xs >>= \x -> ys >>= \y -> return (x,y)

listcomp xs ys = [ (x,y) | x<- xs, y <- ys]

cartesianMonad2 xs ys = xs >>=
                        \x -> ys >>=
                              \y -> return (x,y)

failtry xs = Just xs >>=
             \(_:x:_) -> return x

failtry2 xs = do
  (_:x:_) <- Just xs
  return (x)

failtry3 xs = Just xs >>=
              f
              where f (_:x:_) = return x
                    f _ = fail ""