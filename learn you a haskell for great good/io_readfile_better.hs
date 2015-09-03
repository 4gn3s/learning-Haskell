import System.IO
import Data.Char

--type FilePath = String
--data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

--hGetContents won't attempt to read the file at once and store it in memory,
--but it will read it as needed. That's really cool because we can treat
--contents as the whole contents of the file, but it's not really loaded in memory.
--So if this were a really huge file, doing hGetContents wouldn't choke up our memory,
--but it would read only what it needed to from the file, when it needed to

main1 = do
    handle <- openFile "haiku.txt" ReadMode
    --A value of type Handle represents where our file is
    --openFile. This is its type signature: openFile :: FilePath -> IOMode -> IO Handle.
    --If you read that out loud, it states: openFile takes a file path and an IOMode and
    --returns an I/O action that will open a file and have the file's associated handle encapsulated as its result
    contents <- hGetContents handle
    --hGetContents takes a Handle, so it knows which file to get the contents from
    --and returns an IO String — an I/O action that holds as its result the contents of the file
    --getContents will automatically read from the standard input and
    --hGetContents takes a file handle which tells it which file to read from
    putStr contents
    hClose handle
    --hClose, which takes a handle and returns an I/O action that closes the file.
    --You have to close the file yourself after opening it with openFile

main = do
    withFile "haiku.txt" ReadMode (\handle -> do
    --withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
    --It takes a path to a file, an IOMode and then it takes a function that takes a handle and returns some I/O action
    --it returns an I/O action that will open that file, do something we want with the file and then close it
        contents <- hGetContents handle
        putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

--hGetLine, hPutStr, hPutStrLn, hGetChar, etc.
--They work just like their counterparts without the h,
--only they take a handle as a parameter and operate on that specific file
--instead of operating on standard input or standard output
