import qualified Data.Set as Set

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

--items will be ordered and each item will be unique
set1 = Set.fromList text1
set2 = Set.fromList text2

--lettes in both sets
set3 = Set.intersection set1 set2

--letters that are in the first set but are missing in the second
set4 = Set.difference set1 set2
--this operation is not symmetric
set5 = Set.difference set2 set1

--all the unique letters in both sets
set6 = Set.union set1 set2

--basic functions
--null, size, member, empty, singleton, insert and delete
test1 = Set.null Set.empty == True
test2 = (Set.null $ Set.fromList [3,4,5,5,4,3]) == False
test3 = (Set.size $ Set.fromList [3,4,5,3,4,5]) == 3
test4 = Set.singleton 9 --[9]
test5 = Set.insert 4 $ Set.fromList [9,3,8,1] --[1,3,4,8,9]
test6 = Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5] --[3,5]

--subsets, proper subsets
--Set A is a subset of set B if B contains all the elements that A does
--Set A is a proper subset of set B if B contains all the elements that A does but has more elements
test7 = Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5] == True
test8 = Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5] == False

--map, filter on sets
test9 = Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4] --[3,5,7]
test10 = Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4] --[3,4,5,6,7,8]

--Sets are often used to weed a list of duplicates from a list by first making it into a set with fromList
--and then converting it back to a list with toList. The Data.List function nub already does that,
--but weeding out duplicates for large lists is much faster if you cram them into a set and then convert them back to a list than using nub

setNub xs = Set.toList $ Set.fromList xs
test11 = setNub "HEY WHATS CRACKALACKIN"
