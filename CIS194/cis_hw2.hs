{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
      ("I":t:msg) -> LogMessage Info (read t) (unwords msg)
      ("W":t:msg) -> LogMessage Warning (read t) (unwords msg)
      ("E":n:t:msg) -> LogMessage (Error (read n)) (read t) (unwords msg)
      _ -> Unknown s

test_parseMessage1 :: Bool
test_parseMessage1 = parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

test_parseMessage2 :: Bool
test_parseMessage2 = parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"

test_parseMessage3 :: Bool
test_parseMessage3 = parseMessage "This is not in the right format" == Unknown "This is not in the right format"

test_parseMessage :: Bool
test_parseMessage = test_parseMessage1 && test_parseMessage2 && test_parseMessage3

parse :: String -> [LogMessage]
parse x = [parseMessage l | l <- lines x]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ t _) (Node left cur_msg@(LogMessage _ cur_t _) right)
    | t <= cur_t = Node (insert msg left) cur_msg right
    | otherwise = Node left cur_msg (insert msg right)
insert _ _ = error "we should never get there- Unknown node inserted into the tree"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (l:ls) = insert l (build ls)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

tree :: Int -> IO MessageTree
tree n = build `fmap` testParse parse n "error.log"

inOrderTree :: Int -> IO [LogMessage]
inOrderTree n = inOrder `fmap` (tree n)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getMessage (filter importantError (inOrder (build xs)))
    where
      getMessage (LogMessage _ _ msg) = msg
      getMessage (Unknown _) = ""

      importantError (LogMessage (Error severity) _ _) = severity >= 50
      importantError _ = False

test = testWhatWentWrong parse whatWentWrong "sample.log"
