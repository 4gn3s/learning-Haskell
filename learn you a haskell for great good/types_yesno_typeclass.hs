class YesNo a where
    yesno :: a -> Bool

--some instances of the class:
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

--instance YesNo (Tree a) where
--    yesno EmptyTree = False
--    yesno _ = True

test1 = yesno $ length []
test2 = yesno "haha"
test3 = yesno $ Just 0

--mimicing the if statement with a yesno
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

test4 = yesnoIf [] "YEAH!" "NO!"
test5 = yesnoIf [2,3,4] "YEAH!" "NO!"
test6 = yesnoIf (Just 500) "YEAH!" "NO!"

