--in Prelude, the built-in list is a monad

squares lst = do
  x <- lst
  return (x*x)

pairs l1 l2 = do
  x <- l1
  y <- l2
  return (x, y)

test1 = print $ squares [1,2,3]
--list comprehension is a shortcut for the do monadic notation
test2 = [(x*x) | x <- [1,2,3]]

test3 = print $ pairs [1,2,3] "abc"
test4 = print $ [(x, y) | x <- [1,2,3], y <- "abc"]

--list comprehensions also allow to filter the elements of lists
triples = [(x,y,z) | z <- [1..],
                     x <- [1..z],
                     y <- [x..z],
                     x * x + y * y == z * z]
test5 = print $ take 4 triples
