import Data.Maybe (Maybe)

toList :: String -> [Integer]
toList input = read (" [" ++ input ++ "] " )

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
					[(x, "")] -> Just x
	 				_ -> Nothing

maybeToList :: String -> Maybe [Integer]
maybeToList input = maybeRead $ "[" ++ input ++ "]"

getNumbers :: IO [Integer]
getNumbers = do
	putStr "enter a list of numbers separated by commas\n"
 	input <- getLine
	let maybeList = maybeToList input in
	 	case maybeList of
	 		Just l -> return l
			Nothing -> do
	   			putStr "Try again\n"
	   			getNumbers

main :: IO()
main = do
	list <- getNumbers
 	print $ sum list


-- do
-- 	x <- action1
-- 	y <- action2
--  z <- action3
--
-- can be replaced with
-- action1 >>= (\x ->
-- 	action2 >>= (\y ->
--   action3 >>= (\z ->
-- 	  ...
--)))

test1 = do
	line1 <- getLine
 	line2 <- getLine
  	print (line1 ++ line2)

test2 =
	getLine >>= (\line1 ->
		getLine >>= (\line2 ->
		 print (line1 ++ line2)))
-- do
--  action1
--  action2
--  action3
--
-- can be replaced with
-- action1 >>
--  action2 >>
--   action3

test3 = do
	putStr "ASDF\n"
 	putStr "ZQWC\n"

test4 =
	putStr "ASDF\n" >>
	 putStr "ZQWC\n"

getNumbers' :: IO [Integer]
getNumbers' =
	putStr "enter a list of numbers separated by commas\n" >>
	 getLine >>= (\input ->
		let maybeList = maybeToList input in
	  		case maybeList of
	   			Just l -> return l
	   			Nothing ->
		   			putStr "Try again\n" >>
					 getNumbers'
	  )

main' :: IO()
main' =
	getNumbers >>=
	 \list -> print $ sum list
