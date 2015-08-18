--takes a list of monadic values and evaluates them in sequence, from left to right, collecting all intermediate results into a list
sequence' :: Monad m -> [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = m >>= \ a ->
                        do as <- sequence' ms
                           return (a:as)

sequence'' :: Monad m -> [m a] -> m [a]
sequence'' ms = foldr func (return []) ms
    where
      func :: (Monad m) => m a -> m [a] -> m [a]
      func m acc = do x <- m
                      xs <- acc
                      return (x:xs)

sequence''' :: Monad m -> [m a] -> m [a]
sequence''' [] = return []
sequence''' (m:ms) = do a <- m
                        as <- sequence''' ms
                        return (a:as)

