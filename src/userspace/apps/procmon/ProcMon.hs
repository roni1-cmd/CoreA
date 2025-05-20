-- CoreA Enhanced Process Monitor (Haskell)
-- Creates a process tree and reports status via pipes

module Main where

import Syscall
import Control.Monad (forM_, when)
import Foreign.C.String
import System.Process
import Data.Maybe (isJust)

main :: IO ()
main = do
    -- Check kernel configuration
    conf <- kernelConf
    when (lookup "PROCESS" conf /= Just "1" || lookup "SCHEDULER" conf /= Just "ROUND_ROBIN") $
        sysWrite 1 "Fork not supported" >> sysExit 1

    let iterations = 3
    forM_ [1..iterations] $ \_ -> do
        pipefd <- sysPipe
        case pipefd of
            Nothing -> sysWrite 1 "Pipe failed" >> sysExit 1
            Just (rfd, wfd) -> do
                pid <- sysFork
                case pid of
                    Nothing -> sysWrite 1 "Fork failed" >> sysExit 1
                    Just 0 -> do -- Child
                        gpid <- sysFork
                        case gpid of
                            Just 0 -> do -- Grandchild
                                let msg = "Grandchild PID: " ++ show gpid ++ " Level: 2"
                                sysWrite wfd msg
                                sysExit 0
                            Just _ -> do
                                let msg = "Child PID: " ++ show pid ++ " Level: 1"
                                sysWrite wfd msg
                                sysExit 0
                            Nothing -> sysWrite 1 "Fork failed" >> sysExit 1
                    Just n -> do -- Parent
                        buf <- sysWrite rfd (replicate 100 ' ')
                        msg <- peekCString (castPtr buf)
                        sysWrite 1 msg
                        sysExit 0
    -- Parent process
    pid <- sysFork
    when (isJust pid) $ do
        let msg = "Parent PID: " ++ show (fromMaybe 0 pid) ++ " Level: 0"
        sysWrite 1 msg
    sysExit 0
