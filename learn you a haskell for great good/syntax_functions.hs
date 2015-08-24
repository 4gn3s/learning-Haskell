length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

bmiVal :: (RealFloat a) => a -> a -> a
bmiVal weight height = weight / height ^ 2

bmi :: (RealFloat a) => a -> String
bmi b
  | b <= 18.5 = "underweight"
  | b <= 25.0 = "normal"
  | b <= 30.0 = "fat"
  | otherwise = "whale"


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= fat = "fat"
  | otherwise = "whale"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

head' :: [a] -> a
head' [] = error "no head in empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of
              [] -> error "no head in empty list"
              (x:_) -> x
