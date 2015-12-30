newtype Trace a = Trace ([String], a)

instance Monad Trace where
    (Trace(xs, v)) >>= k =
        let Trace(xs', v') = k v
        in Trace(xs ++ xs', v')
    return x = Trace([], x)

put :: Show a => String -> a -> Trace ()
put msg v = Trace ([msg ++ " " ++ show v], ())

fact :: Integer -> Trace Integer
fact n = do
   put "fact" n
   if n == 0
       then return 1
       else do
           m <- fact (n - 1)
           return (n * m)

main = let Trace (lst, m) = fact 10
       in do
           print lst
           print m
