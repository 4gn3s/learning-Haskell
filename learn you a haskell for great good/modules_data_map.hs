import qualified Data.Map as Map
import Data.Char (isUpper)
--mapy sa zaimplementowane jako drzewa- lepsze niż listy

--fromList function takes an association list (in the form of a list) and returns a map with the same associations
--If there are duplicate keys in the original association list, the duplicates are just discarded
testFromList = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
--fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]

--empty represents an empty map
testEmpty = Map.empty --fromList []

--insert takes a key, a value and a map and returns a new map that's just like the old one, only with the key and value inserted
testInsert = Map.insert 3 100 Map.empty --fromList [(3,100)]

--null checks if a map is empty
testNull = Map.null $ Map.fromList [(2,3),(5,5)] --False

--size reports the size of a map
testSize = Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] --5

--singleton takes a key and a value and creates a map that has exactly one mapping
testSingleton = Map.insert 5 9 $ Map.singleton 3 9 --fromList [(3,9),(5,9)]

--lookup works like the Data.List lookup, only it operates on maps.
--It returns Just something if it finds something for the key and Nothing if it doesn't

--member is a predicate takes a key and a map and reports whether the key is in the map or not
testMember = Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] --True

--map and filter work much like their list equivalents
testMap = Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)] --fromList [(1,100),(2,400),(3,900)]
testFilter = Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')] --fromList [(2,'A'),(4,'B')]

--toList is the inverse of fromList
testToList = Map.toList . Map.insert 9 2 $ Map.singleton 4 3 --[(4,3),(9,2)]

--keys and elems return lists of keys and values respectively

--fromListWith acts like fromList, only it doesn't discard duplicate keys
--but it uses a function supplied to it to decide what to do with them

phoneBook = [("betty","555-2938"), ("betty","342-2492"), ("bonnie","452-2928"), ("patsy","493-2928"), ("patsy","943-2929"), ("patsy","827-9162"), ("lucille","205-2928"), ("wendy","939-8282"), ("penny","853-2492"),("penny","555-2111")]

--if there are duplicates in the list, it concatenates them into a string
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

testFromListWith = Map.lookup "patsy" $ phoneBookToMap phoneBook --Just "827-9162, 943-2929, 493-2928"

--in case of dupicates, they are added to a list
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

testFromListWith1 = Map.lookup "patsy" $ phoneBookToMap' phoneBook --Just ["827-9162","943-2929","493-2928"]

--choose maximum value for the same keys
testFromListWith2 = Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] --fromList [(2,100),(3,29),(4,22)]

--sum values for the same keys
testFromListWith3 = Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] --fromList [(2,108),(3,62),(4,37)]

--insertWith inserts a key-value pair into a map, but if that map already contains the key, it uses the function passed to it to determine what to do
testInsertWith = Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)] --fromList [(3,104),(5,103),(6,339)]
