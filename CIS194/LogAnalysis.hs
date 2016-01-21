module LogAnalysis where
    import Log

    parseMessage :: String -> LogMessage
    parseMessage str = case (words str) of
        ("E":e:c:msg) -> LogMessage (Error (read e)) (read c) (unwords msg)
        ("I":c:msg) -> LogMessage Info (read c) (unwords msg)
        ("W":c:msg) -> LogMessage Warning (read c) (unwords msg)
        _ -> Unknown str

    test1 = parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
    test2 = parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    test3 = parseMessage "This is not right" == Unknown "This is not right"

    parse :: String -> [LogMessage]
    parse str = map parseMessage $ lines str

    logs = testParse parse 10 "error.log"

    insert :: LogMessage -> MessageTree -> MessageTree
    insert (Unknown _) tree = tree
    insert msg Leaf = Node Leaf msg Leaf
    insert msg@(LogMessage _ timestamp _) (Node left m@(LogMessage _ t _) right) =
        if timestamp <= t
            then Node (insert msg left) m right
            else Node left m (insert msg right)

    build :: [LogMessage] -> MessageTree
    build [] = Leaf
    build (x:xs) = insert x (build xs)

    inOrder :: MessageTree -> [LogMessage]
    inOrder Leaf = []
    inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

    -- test5 = inOrder (build tree)
    whatWentWrong :: [LogMessage] -> [String]
    whatWentWrong xs = map getMessage $ filter important $ inOrder $ build xs
        where important (LogMessage (Error severity) _ _) = severity >= 50
              important _ = False

              getMessage (LogMessage _ _ msg) = msg

    test5 = testWhatWentWrong parse whatWentWrong "sample.log"
