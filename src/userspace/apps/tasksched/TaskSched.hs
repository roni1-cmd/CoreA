-- CoreA Task Scheduler (Haskell)
-- Spawns tasks, synchronizes with semaphore, reports via pipes

module Main where

import Syscall
import Control.Monad (forM_, when)
import Foreign.C.String
import Data.Maybe (isJust)

main :: IO ()
main = do
    -- Check kernel configuration
    conf <- kernelConf
    when (lookup "PROCESS" conf /= Just "1" || lookup "SCHEDULER" conf /= Just "ROUND_ROBIN" || lookup "IPC_SEMAPHORE" conf /= Just "1" || lookup "IPC_PIPE" conf /= Just "1") $
        sysWrite 1 "Required features disabled" >> sysExit 1

    -- Create semaphore
    semId <- sysSemaphore 0
    case semId of
        Nothing -> sysWrite 1 "Semaphore failed" >> sysExit 1
        Just sid -> do
            let tasks = 3
            forM_ [1..tasks] $ \i -> do
                pipefd <- sysPipe
                case pipefd of
                    Nothing -> sysWrite 1 "Pipe failed" >> sysExit 1
                    Just (rfd, wfd) -> do
                        pid <- sysFork
                        case pid of
                            Nothing -> sysWrite 1 "Fork failed" >> sysExit 1
                            Just 0 -> do -- Child
                                let sum = sum [1..100] + i
                                let msg = "Task " ++ show i ++ " PID: " ++ show pid ++ " Sum: " ++ show sum
                                sysWrite wfd msg
                                sysSemaphore sid
                                sysExit 0
                            Just _ -> do -- Parent
                                buf <- sysWrite rfd (replicate 100 ' ')
                                msg <- peekCString (castPtr buf)
                                sysWrite 1 msg
            -- Release semaphore
            sysSemaphore 1
            sysWrite 1 "Scheduler completed"
            sysExit 0
