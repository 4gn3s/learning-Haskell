--takes a list of monadic values and evaluates them into a sequence from left to right ignoring intermediate results
sequence_' :: Monad m -> [m a] -> m ()
sequence_' [] = return ()
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

sequence_'' :: Monad m -> [m a] -> m ()
sequence_''[] = return ()
sequence_'' (m:ms) = m >> sequence_'' ms

sequence_''' :: Monad -> [m a] -> m ()
sequence_''' [] = return ()
sequence_''' (m:ms) = m >>= \_ -> sequence_''' ms

sequence_'''' :: Monad m -> [m a] -> m ()
sequence_'''' ms = foldr (>>) (return ()) ms

