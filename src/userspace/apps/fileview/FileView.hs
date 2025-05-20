-- CoreA File Viewer (Haskell)
-- Reads and displays file contents

module Main where

import Syscall
import System.IO

main :: IO ()
main = do
    contents <- readFile "test.txt" `catch` (\_ -> return "No data")
    sysWrite 1 contents
    sysExit 0
