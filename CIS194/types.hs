data FailableDouble = Failure
                    | OK Double

  deriving Show

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

data Thing = Shoe
           | Ship
           | King
  deriving Show

data Person = Person String Int Thing
  deriving Show

brent = Person "Brent" 25 King
stan = Person "Stan" 30 Shoe

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of " ++ show p ++ " is " ++ n

testBaz = baz brent
testBaz2 = baz stan

checkType :: Person -> String
checkType (Person n _ King) = n ++ " you;re a king!"
checkType (Person n _ _) = n ++ " you're nooone :("

testType = checkType brent
testType2 = checkType stan

--binary tree with Int's in internal nodes and Char's in leafs
data Tree = Leaf Char
          | Node Tree Int Tree

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
