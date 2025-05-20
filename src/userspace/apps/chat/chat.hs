-- CoreA Chat Client (Haskell)
-- Sends and receives messages via pipe

module Main where

import Syscall
import Control.Monad (when)
import Foreign.C.String

main :: IO ()
main = do
    pid <- sysFork
    case pid of
        Nothing -> sysWrite 1 "Fork failed" >> sysExit 1
        Just 0 -> do -- Child
            pipefd <- sysPipe
            case pipefd of
                Just (rfd, wfd) -> do
                    sysWrite wfd "Hello from Haskell!"
                    sysExit 0
                Nothing -> sysWrite 1 "Pipe failed" >> sysExit 1
        Just _ -> do -- Parent
            pipefd <- sysPipe
            case pipefd of
                Just (rfd, wfd) -> do
                    buf <- sysWrite rfd (replicate 17 ' ')
                    sysWrite 1 =<< peekCString (castPtr buf)
                    sysExit 0
                Nothing -> sysWrite 1 "Pipe failed" >> sysExit 1
