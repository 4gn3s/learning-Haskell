phoneBook :: [(String,String)]
phoneBook = [("betty","555-2938"), ("bonnie","452-2928"), ("patsy","493-2928"), ("lucille","205-2928"), ("wendy","939-8282"), ("penny","853-2492")]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
test1 = Right 20
test2 = Left "w00t"
