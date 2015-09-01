import Data.Char
import Data.List (groupBy)
import Data.Function (on)
--isControl checks whether a character is a control character

--isSpace checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc

--isLower checks whether a character is lower-cased

--isUpper checks whether a character is upper-cased

--isAlpha checks whether a character is a letter

--isAlphaNum checks whether a character is a letter or a number

--isPrint checks whether a character is printable. Control characters, for instance, are not printable

--isDigit checks whether a character is a digit

--isOctDigit checks whether a character is an octal digit

--isHexDigit checks whether a character is a hex digit

--isLetter checks whether a character is a letter

--isMark checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French

--isNumber checks whether a character is numeric

--isPunctuation checks whether a character is punctuation

--isSymbol checks whether a character is a fancy mathematical or currency symbol

--isSeparator checks for Unicode spaces and separators

--isAscii checks whether a character falls into the first 128 characters of the Unicode character set

--isLatin1 checks whether a character falls into the first 256 characters of Unicode

--isAsciiUpper checks whether a character is ASCII and upper-case

--isAsciiLower checks whether a character is ASCII and lower-case

testIsSpace = groupBy ((==) `on` isSpace) "hey guys its me" --["hey"," ","guys"," ","its"," ","me"]
--this acts just like words from Data.List but leaves spaces " "; fix:
testIsSpaceFilter = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me" --["hey","guys","its","me"]

--toUpper converts a character to upper-case. Spaces, numbers, and the like remain unchanged

--toLower converts a character to lower-case

--toTitle converts a character to title-case. For most characters, title-case is the same as upper-case

--digitToInt converts a character to an Int. To succeed, the character must be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'
testDigitToInt = map digitToInt "34538" --[3,4,5,3,8]

--intToDigit is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character
testIntToDigit = intToDigit 5 --'5'

--The ord and chr functions convert characters to their corresponding numbers and vice versa
testOrd = ord 'a' --97
testChr = chr 97 --'a'
