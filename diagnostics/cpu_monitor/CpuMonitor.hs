-- CoreA CPU Monitor (Haskell)
-- Tracks CPU usage via process activity

module Main where

import Syscall
import Control.Monad (forM_, when)
import Data.Maybe (isJust)

main :: IO ()
main = do
    -- Check kernel configuration
    conf <- kernelConf
    when (lookup "PROCESS" conf /= Just "1" || lookup "SCHEDULER" conf /= Just "ROUND_ROBIN" || lookup "IPC_SEMAPHORE" conf /= Just "1") $
        sysWrite 1 "Required features disabled" >> sysExit 1

    -- Create semaphore
    semId <- sysSemaphore 0
    case semId of
        Nothing -> sysWrite 1 "Semaphore failed" >> sysExit 1
        Just sid -> do
            let samples = 5
            forM_ [1..samples] $ \_ -> do
                pid <- sysFork
                case pid of
                    Nothing -> sysWrite 1 "Fork failed" >> sysExit 1
                    Just 0 -> do -- Child
                        let load = sum [1..1000]
                        sysSemaphore sid
                        sysExit 0
                    Just _ -> pure ()
            -- Report usage
            let msg = "CPU Usage Sampled Samples: " ++ show samples
            sysWrite 1 msg
            sysSemaphore 1
            sysExit 0
