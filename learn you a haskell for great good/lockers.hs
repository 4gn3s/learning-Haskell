import qualified Data.Map as Map
import Prelude hiding (Either, Left, Right)

data Either a b = Left a
                | Right b
    deriving (Eq, Ord, Read, Show)

data LockerState = Taken
                 | Free
    deriving (Eq, Show)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

--simulating high school lockers
--each locker has a code
--if locker is not taken, the student may get the code
--if locker is taken, the code is secret

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNr lockerMap = case Map.lookup lockerNr lockerMap of
    Nothing -> Left $ "Locker " ++ show lockerNr ++ " doesn't exist"
    Just (state, code) -> if state == Taken
                          then
                            Left $ "Locker " ++ show lockerNr ++ " is already taken!"
                          else
                            Right code


lockers :: LockerMap
lockers = Map.fromList[(100,(Taken,"ZD39I")), (101,(Free,"JAH3I")), (103,(Free,"IQSA9")), (105,(Free,"QOTSA")), (109,(Taken,"893JJ")), (110,(Taken,"99292"))]

test1 = lockerLookup 101 lockers
test2 = lockerLookup 100 lockers
test3 = lockerLookup 102 lockers
