module Calculator where

    newtype Calc = Calc [Int]
        deriving Show

    type Action a = Calc -> (Calc, a)

    newtype Calculation a = CL (Action a)

    pushCalc n = CL ( \(Calc lst) -> (Calc (n:lst), ()) )
    popCalc = CL ( \(Calc lst) -> let x:xs = lst
                                  in (Calc xs, x) )
    addCalc = CL ( \(Calc lst) -> let a:b:xs = lst
                                  in (Calc (a+b:xs), ()) )

    instance Monad Calculation where
        return x = CL (\calc -> (calc, x))
        CL(c) >>= cont =
            CL (\calc -> let (calc', v) = c calc
                             CL c' = cont v
                         in c' calc')

    add x y = do
        pushCalc x
        pushCalc y
        addCalc
        r <- popCalc
        return r

    eval :: Calculation a -> Calc -> a
    eval (CL f) calc = snd $ f calc

    main = let calc = Calc []
           in  print $ eval (add 2 3) calc
