greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (>100) xs

test1 = greaterThan100 [1,9,349,6,907,98,105] == [349,907,105]

myTest :: [Integer] -> Bool
myTest = even . length . greaterThan100

test2 = myTest [1,9,349,6,907,98,105]
