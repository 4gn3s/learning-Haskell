import Data.List
import System.Environment
import System.IO

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show, Eq)
type Path = [(Label, Int)]

--for the given paths, finds the cheapest way to perform a next step forward
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA then (A, a) : pathA else (C, c) : (B, b) : pathB
        newPathToB = if forwardPriceToB <= crossPriceToB then (B, b) : pathB else (C, c) : (A, a) : pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath system =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) system
    in
        if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath

test1 = optimalPath heathrowToLondon == [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]

--dividing a list into multiple lists of length n
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

test2 = groupsOf 3 [1,2,3,4,5,6] == [[1,2,3], [4,5,6]]

main = do
    (fileName : _) <- getArgs
    contents <- readFile fileName
    let threes = groupsOf 3 (map read $ lines contents)
        system = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath system
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The optimal path is " ++ pathString
    putStrLn $ "The optimal paths price is " ++ show pathPrice
