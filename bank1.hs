deposit :: (Num a) => a -> a -> Maybe a
deposit value account = Just (account + value)

withdraw :: (Num a, Ord a) => a -> a -> Maybe a
withdraw value account = if account < value
							then Nothing
	   					 else Just (account - value)

eligible :: (Num a, Ord a) => a -> Maybe Bool
eligible a = do
	a1 <- deposit 100 a
 	a2 <- withdraw 200 a1
  	a3 <- deposit 100 a2
	a4 <- withdraw 300 a3
	a5 <- deposit 1000 a4
 	Just True

main = do
	print $ eligible 300
 	print $ eligible 299

eligible' :: (Num a, Ord a) => a -> Maybe Bool
eligible' a =
	deposit 100 a >>=
	 withdraw 200 >>=
	  deposit 100 >>=
	   withdraw 300 >>=
		deposit 1000 >>
	  	return True

main' = do
	print $ eligible' 300
 	print $ eligible' 299
