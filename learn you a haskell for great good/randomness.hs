import System.Random

--problem with randomness in haskell- since it is pure functional
--and therefore functions should always return the same output

--random :: (RandomGen g, Random a) => g -> (a, g)
--RandomGen typeclass is for types that can act as sources of randomness
--The Random typeclass is for things that can take on random values
--StdGen that is an instance of the RandomGen typeclass
--We can either make a StdGen manually or we can tell the system to give us one based on a multitude of sort of random stuff
--To manually make a random generator, use the mkStdGen function. It has a type of mkStdGen :: Int -> StdGen

--simulating throwing 3 coins
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

test1 = threeCoins (mkStdGen 21)
test2 = threeCoins (mkStdGen 22)

--what if we wanted to throw 5 coins?
--there's a function called randoms that takes a generator
--and returns an infinite sequence of values based on that generator
test3 = take 5 $ randoms (mkStdGen 11) :: [Bool]
test4 = take 5 $ randoms (mkStdGen 11) :: [Float]

--What if we want a random value in some sort of range?
--randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)
--like random, only it takes as its first parameter a pair of values
--that set the lower and upper bounds and the final value produced will be within those bounds

--test5 = randomR (1,6) (mkStdGen 359353)

--randomRs, which produces a stream of random values within our defined ranges
test6 = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

--getStdGen I/O action, which has a type of IO StdGen.
--When your program starts, it asks the system for a good random number
--generator and stores that in a so called global generator

--generates a random string
main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)
