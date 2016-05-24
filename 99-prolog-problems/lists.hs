-- 1.01 (*) Find the last element of a list.
find_last_in_list :: [a] -> a
find_last_in_list [] = error "no last in empty list"
find_last_in_list (x:xs) = case xs of
        [] -> x
        otherwise -> find_last_in_list xs

-- 1.02 (*) Find the last but one element of a list.
find_second_last_in_list :: [a] -> a
find_second_last_in_list [] = error "no second last in an empty list"
find_second_last_in_list [x] = error "no second last in 1-element list"
find_second_last_in_list [x, y] = x
find_second_last_in_list (x:xs) = find_second_last_in_list xs

-- 1.03 (*) Find the K'th element of a list.
find_kth_of_list :: [a] -> Int -> a
find_kth_of_list [] _ = error "no kth element in an empty list"
find_kth_of_list xs k = find_kth_of_list_indexed xs k 0

find_kth_of_list_indexed :: [a] -> Int -> Int -> a
find_kth_of_list_indexed [] k index = error "k too high, element does not exist"
find_kth_of_list_indexed (x:xs) k index = if k == index then x else find_kth_of_list_indexed xs k (index + 1)

-- 1.04 (*) Find the number of elements of a list.
get_length :: [a] -> Int
get_length [] = 0
get_length (x:xs) = 1 + get_length xs

-- 1.05 (*) Reverse a list.
reverse_list :: [a] -> [a]
reverse_list [] = []
reverse_list (x:xs) = reverse_list xs ++ [x]

-- 1.06 (*) Find out whether a list is a palindrome.
is_list_a_palindrome :: Eq a => [a] -> Bool
is_list_a_palindrome xs
    | len <= 1 = len == 1
    | len == 2 = firstChar == lastChar
    | firstChar /= lastChar = False
    | firstChar == lastChar = is_list_a_palindrome cutList
    where
        len = length xs
        firstChar = head xs
        lastChar = last xs
        cutList = init $ tail xs

-- 1.07 (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten_list :: NestedList a -> [a]
flatten_list (Elem x) = [x]
flatten_list (List x) = concatMap flatten_list x

test_flatten = flatten_list (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]

-- 1.08 (**) Eliminate consecutive duplicates of list elements.
remove_consecutive_duplicates :: Eq a => [a] -> [a]
remove_consecutive_duplicates [] = []
remove_consecutive_duplicates [x] = [x]
remove_consecutive_duplicates (x:xs) = remove_consecutive_duplicates_first x xs

remove_consecutive_duplicates_first :: Eq a => a -> [a] -> [a]
remove_consecutive_duplicates_first y [] = [y]
remove_consecutive_duplicates_first y (x:xs) = if x == y then remove_consecutive_duplicates_first x xs else [y] ++ remove_consecutive_duplicates_first x xs

-- 1.09 (**) Pack consecutive duplicates of list elements into sublists.
pack_consecutive_duplicates_into_sublists :: Eq a => [a] -> [[a]]
pack_consecutive_duplicates_into_sublists [] = []
pack_consecutive_duplicates_into_sublists (x:xs) = (x:first) : pack_consecutive_duplicates_into_sublists rest
    where
        getDups [] = ([], [])
        getDups (y:ys)
            | y == x = let (f, r) = getDups ys in (y:f, r)
            | otherwise = ([], (y:ys))
        (first, rest) = getDups xs

-- 1.10 (*) Run-length encoding of a list.
run_encode_list :: Eq a => [a] -> [(a, Int)]
run_encode_list = map (\x -> (head x, length x)) . pack_consecutive_duplicates_into_sublists

-- 1.11 (*) Modified run-length encoding (if an element has no duplicates it is simply copied into the result list)
data ListItem a = Single a | Multiple Int a
    deriving (Show)

run_encode_list_mod :: Eq a => [a] -> [ListItem a]
run_encode_list_mod = map clean . run_encode_list
    where
        clean (x, 1) = Single x
        clean (x, n) = Multiple n x

-- 1.12 (**) Decode a run-length encoded list (from 1.11).
decode_run_encoded_list :: Eq a => [ListItem a] -> [a]
decode_run_encoded_list [] = []
decode_run_encoded_list (x:xs) =
    case x of
        Single y -> y : decode_run_encoded_list xs
        Multiple n y -> replicate n y ++ decode_run_encoded_list xs

-- 1.13 (**) Run-length encoding of a list (direct solution).
run_encode_list_dir :: Eq a => [a] -> [(a, Int)]
run_encode_list_dir [] = []
run_encode_list_dir (x:xs) = run_encode_list_helper xs x 1

run_encode_list_helper :: Eq a => [a] -> a -> Int -> [(a, Int)]
run_encode_list_helper [] x n = [(x, n)]
run_encode_list_helper (x:xs) y n = if x == y then run_encode_list_helper xs x (n+1) else (y, n) : run_encode_list_helper xs x 1

-- 1.14 (*) Duplicate the elements of a list.
duplicate_elements :: [a] -> [a]
duplicate_elements [] = []
duplicate_elements [x] = [x, x]
duplicate_elements (x:xs) = [x, x] ++ duplicate_elements xs

-- 1.15 (**) Duplicate the elements of a list a given number of times.
duplicate_elements_k :: [a] -> Int -> [a]
duplicate_elements_k [] _ = []
duplicate_elements_k (x:xs) k = replicate k x ++ duplicate_elements_k xs k

-- 1.16 (**) Drop every N'th element from a list.
