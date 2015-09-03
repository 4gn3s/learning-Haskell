import Data.Char

--program continuously reads a line and then tells us if the line is a palindrome or not
--using interact:
--we have to replace each line of the input with either "palindrome" or "not a palindrome".
--So we have to write a function that transforms something like
--"elephant\nABCBA\nwhatever" into "not a palindrome\npalindrome\nnot a palindrome"

parsePalindromes :: String -> String
parsePalindromes = unlines . map (\x -> if isPalindrome x then "is palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs

main = interact parsePalindromes
