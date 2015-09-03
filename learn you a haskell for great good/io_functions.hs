import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
