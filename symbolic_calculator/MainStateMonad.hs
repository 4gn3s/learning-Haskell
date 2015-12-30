module Main where

import qualified Data.Map as M --qualified means that the namespace is accessible through Data.Map.<name>

import Lexer (tokenize)
import Parser (parse)
import EvaluatorStateMonad

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
		    Ev act = evaluate tree
		    (val, symTab') = act symTab
		in do
		    print val
		    print symTab'
		    loop symTab'
