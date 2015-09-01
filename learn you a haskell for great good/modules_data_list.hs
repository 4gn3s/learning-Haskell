import Data.List
import Data.Function (on)

--nub is a function defined in Data.List that takes a list and weeds out duplicate elements
numUniques :: Eq a => [a] -> Int
numUniques = length . nub

--intersperse takes an element and a list and then puts that element in between each pair of elements in the list
testIntersperse = intersperse '.' "MONKEY" --"M.O.N.K.E.Y"

--intercalate takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.
testIntercalate = intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]] --[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

--transpose transposes a list of lists. If you look at a list of lists as a 2D matrix, the columns become the rows and vice versa.
testTranspose1 = transpose [[1,2,3],[4,5,6],[7,8,9]] --[[1,4,7],[2,5,8],[3,6,9]]
testTranspose2 = transpose ["hey","there","guys"] --["htg","ehu","yey","rs","e"]

--concat flattens a list of lists into just a list of elements.
testConcat = concat [[3,4,5],[2,3,4],[2,1,1]] --[3,4,5,2,3,4,2,1,1]

--concatMap is the same as first mapping a function to a list and then concatenating the list with concat
testConcatMap = concatMap (replicate 4) [1..3] --[1,1,1,1,2,2,2,2,3,3,3,3]

--and takes a list of boolean values and returns True only if all the values in the list are True
testAnd = and $ map (==4) [4,4,4,3,4] --False

--or is like and, only it returns True if any of the boolean values in a list is True
testOr = or $ map (==4) [2,3,4,5,6,1] --True

--any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively
testAny = any (==4) [2,3,5,6,1,4] --True
testAll = all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" --False

--iterate takes a function and a starting value. It applies the function to the starting value,
--then it applies that function to the result, then it applies the function to that result again, etc.
--It returns all the results in the form of an infinite list
testIterate = take 10 $ iterate (*2) 1 --[1,2,4,8,16,32,64,128,256,512]

--splitAt takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple
testSplitAt = splitAt 3 "heyman" --("hey","man")

--takeWhile takes elements from a list while the predicate holds and then
--when an element is encountered that doesn't satisfy the predicate, it's cut off
testTakeWhile = takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] --[6,5,4]

--dropWhile is similar, only it drops all the elements while the predicate is true
testDropWhile = dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1] --[3,4,5,4,3,2,1]

--span is kind of like takeWhile, only it returns a pair of lists: the output of takeWhile and the part that would have been dropped
testSpan = span (/=4) [1,2,3,4,5,6,7] --([1,2,3],[4,5,6,7])

--break p is the equivalent of doing span (not . p)
testBreak = break (==4) [1,2,3,4,5,6,7] --([1,2,3],[4,5,6,7])

--sort simply sorts a list (the list has to be part of the Ord typeclass)
testSort = sort [8,5,3,2,1,6,4,2] --[1,2,2,3,4,5,6,8]

--group takes a list and groups adjacent elements into sublists if they are equal
testGroup = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] --[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

--to figure out how many times each element occurs in the list, we can sort it and then group it
testElementsCounts = map (\l@(x:xs) -> (x, length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

--inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left
testInits = inits "w00t" --["","w","w0","w00","w00t"]
testTails = tails "w00t" --["w00t","00t","0t","t",""]
testSplitWord = let w = "w00t" in zip (inits w) (tails w) --[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]

--isInfixOf searches for a sublist within a list
testIsInfixOf1 = "cat" `isInfixOf` "im a cat burglar" --True
testIsInfixOf2 = "Cat" `isInfixOf` "im a cat burglar" --False

--isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively
testIsPrefixOf = "hey" `isPrefixOf` "hey there!" --True
testIsSuffixOf = "there!" `isSuffixOf` "oh hey there!" --True

--elem and notElem check if an element is or isn't inside a list

--partition takes a list and a predicate and returns a pair of lists.
--The first list in the result contains all the elements that satisfy the predicate,
--the second contains all the ones that don't.
testPartition = partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" --("BOBMORGAN","sidneyeddy")
--span takes only the first part, while partition takes all!
testSpan2 = span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" --("BOB","sidneyMORGANeddy")

--find takes a list and a predicate and returns the first element that satisfies the predicate
--returns that element wrapped in a Maybe value
testFind1 = find (>4) [1,2,3,4,5,6] --Just 5
testFind2 = find (>9) [1,2,3,4,5,6] --Nothing

--elemIndex is kind of like elem, only it doesn't return a boolean value. It maybe returns the index of the element we're looking for
testElemIndex1 = 4 `elemIndex` [1,2,3,4,5,6] --Just 3
testElemIndex2 = 10 `elemIndex` [1,2,3,4,5,6] --Nothing

--elemIndices is like elemIndex, only it returns a list of indices (or [])
testElemIndices = ' ' `elemIndices` "Where are the spaces?" --[5,9,13]

--findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate
testFindIndex = findIndex (==4) [5,3,2,1,6,4] --Just 5

--findIndices returns the indices of all elements that satisfy the predicate in the form of a list
testFindIndices = findIndices (`elem` ['A'..'Z']) "Where Are The Caps?" --[0,6,10,14]

--zipN N<=7 to zip N lists together
testZipN = zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2] --[(2,2,5,2),(3,2,5,2),(3,2,3,2)]

--zipWithN N<=7: takes a function to preform zipping to a single value
testZipWithN = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3] --[7,9,8]

--lines takes a string and returns every line of that string in a separate list
testLines = lines "first line\nsecond line\nthird line" --["first line","second line","third line"]

--unlines takes a list of strings and joins them together using a '\n'
testUnlines = unlines ["first line", "second line", "third line"] --"first line\nsecond line\nthird line\n"

--words and unwords are for splitting a line of text into words or joining a list of words into a text
testWords = words "hey these           are    the words in this\nsentence" --["hey","these","are","the","words","in","this","sentence"]
testUnwords = unwords ["hey","there","mate"] --"hey there mate"

--nub takes a list and weeds out the duplicate elements, returning a list whose every element is unique
testNub = nub [1,2,3,4,3,2,1,2,3,4,3,2,1] --[1,2,3,4]

--delete takes an element and a list and deletes the first occurence of that element in the list
testDelete = delete 'h' "hey there ghang!" --"ey there ghang!"

-- \\ is the list difference function. It acts like a set difference, basically.
--For every element in the right-hand list, it removes a matching element in the left one
testDifference = [1..10] \\ [2,5,9] --[1,3,4,6,7,8,10]

--union returns the union of two lists
testUnion = [1..7] `union` [5..10] --[1,2,3,4,5,6,7,8,9,10]

--intersect works like set intersection. It returns only the elements that are found in both lists
testIntersect = [1..7] `intersect` [5..10] --[5,6,7]

--insert takes an element and a list of elements that can be sorted and inserts it
--into the last position where it's still less than or equal to the next element
testInsert = insert 4 [1,2,3,5,6,7] --[1,2,3,4,5,6,7]

--The nub, delete, union, intersect and group functions all have their more general counterparts called
--nubBy, deleteBy, unionBy, intersectBy and groupBy
--group is the same as groupBy (==)
values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
testGroupBy = groupBy (\x y -> (x > 0) == (y > 0)) values
--[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

--Similarly, the sort, insert, maximum and minimum also have their more general equivalents.
--Functions like groupBy take a function that determines when two elements are equal.
--sortBy, insertBy, maximumBy and minimumBy take a function that determine if one element is greater, smaller or equal to the other
xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
testSortBy = sortBy (compare `on` length) xs
--[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

--module Data.Map has function filter (as Prelude has one)
--if we just import it, the names clash
--we can do:
--import qualified Data.Map as M
--and then use M.filter(..)
--or do
--import qualified Data.Map
--and use Data.Map.filter
