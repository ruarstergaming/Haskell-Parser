module Main where

import Parsing
import Expr
import REPL
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then
        repl initLState else
            case parse pCommand ("file \0" ++ head args ++ "\0") of
                [(cmd, "")] -> process initLState (cmd:[Quit]) -- quits system once file has concluded - allows for execution of text files as scripts
                _           -> error "Parse Error"
