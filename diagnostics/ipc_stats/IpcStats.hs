-- CoreA IPC Stats (Haskell)
-- Measures pipe throughput

module Main where

import Syscall
import Control.Monad (forM_)

main :: IO ()
main = do
    -- Check kernel configuration
    conf <- kernelConf
    when (lookup "IPC_PIPE" conf /= Just "1") $
        sysWrite 1 "Pipe not supported" >> sysExit 1

    -- Create pipe
    pipefd <- sysPipe
    case pipefd of
        Nothing -> sysWrite 1 "Pipe failed" >> sysExit 1
        Just (rfd, wfd) -> do
            pid <- sysFork
            case pid of
                Nothing -> sysWrite 1 "Fork failed" >> sysExit 1
                Just 0 -> do -- Child
                    let iterations = 1000
                    forM_ [1..iterations] $ \_ ->
                        sysWrite wfd "x"
                    sysExit 0
                Just _ -> do -- Parent
                    let iterations = 1000
                    forM_ [1..iterations] $ \_ ->
                        sysWrite rfd (replicate 1 ' ')
                    let msg = "IPC Throughput: " ++ show iterations ++ " bytes"
                    sysWrite 1 msg
                    sysExit 0
