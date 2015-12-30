module Main where

import qualified Data.Map as M --qualified means that the namespace is accessible through Data.Map.<name>

import Lexer (tokenize)
import Parser (parse)
import Evaluator

main :: IO()
main = do
	loop (M.fromList[("pi", pi), ("e", exp 1)])

loop symTab = do
	str <- getLine
	if null str
	then return ()
	else
		let toks = tokenize str
		    tree = parse toks
		    Ev ev = evaluate tree symTab
		in
				case ev of
					Left msg -> do
						putStrLn $ "Error: " ++ msg
						loop symTab -- use old symTab
					Right (v, symTab') ->	do
						print v
						print symTab'
						loop symTab'
