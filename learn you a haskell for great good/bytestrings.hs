import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import System.Environment

--bytestrings are sort of like lists, only each element is one byte (or 8 bits) in size
--Bytestrings come in two flavors: strict and lazy ones

--pack has the type signature pack :: [Word8] -> ByteString.
--What that means is that it takes a list of bytes of type Word8 and returns a ByteString.
--You can think of it as taking a list, which is lazy, and making it less lazy
--Word8: it's like Int, only that it has a much smaller range, namely 0-255
test1 = B.pack [99,97,110]
test2 = B.pack [98..120]

--unpack  bytestring and turns it into a list of bytes
--fromChunks takes a list of strict bytestrings and converts it to a lazy bytestring
--toChunks takes a lazy bytestring and converts it to a list of strict ones
test3 = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]

--cons takes a byte and a bytestring and puts the byte at the beginning. It's lazy
--it will make a new chunk even if the first chunk in the bytestring isn't full
--therefore it's better to use the strict version of cons, cons' if you're going to be
--inserting a lot of bytes at the beginning of a bytestring
test4 = B.cons 85 $ B.pack [80,81,82,84]
test5 = B.cons' 85 $ B.pack [80,81,82,84]
test6 = foldr B.cons B.empty [50..60]
test7 = foldr B.cons' B.empty [50..60]

--empty makes an empty bytestring


--program to copy the contents of the file given in the first argument
--to the file given by the second argument
--using bytestrings (lazy, because otherwise it will read the whole file into memory at once!)
main = do
    (fileName1 : fileName2 : _) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
    contents <- B.readFile src
    B.writeFile dst contents

