import Prelude hiding (Functor, fmap)

--Functor typeclass, which is basically for things that can be mapped over
--list type is part of the Functor typeclass (mapping over a list)

class Functor f where
    fmap :: (a -> b) -> f a -> f b
--f is not a concrete type (a type that a value can hold, like Int, Bool or Maybe String),
--but a type constructor that takes one type parameter
--aybe Int is a concrete type, but Maybe is a type constructor that takes one type as the parameter

--fmap takes a function from one type to another and a functor applied with one type and returns a functor applied with another type


--map :: (a -> b) -> [a] -> [b]
instance Functor [] where
    fmap = map

test1 = fmap (*2) [1..3] == map (*2) [1..3]

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

test2 = fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
--data Either a b = Left a | Right b
--if we wanted to map one function over both of them, a and b would have to be the same type
