module MonadicCalculator where
import Prelude hiding (return)

newtype Calc = Calc [Int]
    deriving Show

popCalc :: Calc -> (Calc, Int)
--popCalc takes no arguments
--popCalc returns a promise to calculate the top of the calculator’s stack when the stack is available
popCalc = \(Calc lst) -> (Calc (tail lst), head lst)

-- testing popCalc:
-- let f = popCalc
-- let calc = Calc [1,2,3]
-- let (calc', x) = f calc
-- > (Calc [2,3], 1)

pushCalc :: Int -> Calc -> (Calc, ())
pushCalc n = \(Calc lst) -> (Calc (n:lst), ())

addCalc :: Calc -> (Calc, ())
addCalc = \(Calc lst) -> let (a:b:xs) = lst
                     in (Calc (a+b:xs), ())

add :: Int -> Int -> Int
--some awesome code coming:
add x y = let px = pushCalc x
              py = pushCalc y
              sumxy = addCalc
              pp = popCalc
              --up to this point we defined promises of what to do,
              --now we're actually performing them
              calc = Calc []
              (calc1, _) = px calc
              (calc2, _) = py calc1
              (calc3, _) = sumxy calc2
              (_, z) = pp calc3
           in z

--what we really want, is a function
-- bind :: (Calc -> (Calc, a)) ->        -- action
--         (a -> (Calc -> (Calc, b)) ->  -- continuation
--         (Calc -> (Calc, b))           -- new action

type Action a = Calc -> (Calc, a)
--then the signature can be pretty simple

bind :: (Action a) -> (a -> (Action b)) ->  (Action b)
bind act cont = \calc -> let (calc', v) = act calc -- v is of type a
                             act' = cont v
                         in act' calc' -- applying the new action to the new calculator

return :: a -> Action a
return v = \calc -> (calc, v)

--we have just defined a generic state monad
--nothing here depends on Calc functions

--this doesn't work, we need to define a monad as a type
-- add' :: Int -> Int -> Int
-- add' x y = do
--         pushCalc x
--         pushCalc y
--         addCalc
--         r <- popCalc
--         return r

main :: IO()
main = do
    putStrLn "Calc"
