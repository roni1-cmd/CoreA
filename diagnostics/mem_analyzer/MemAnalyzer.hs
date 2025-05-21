-- CoreA Memory Analyzer (Haskell)
-- Reports shared memory usage

module Main where

import Syscall
import Control.Monad (when)

main :: IO ()
main = do
    -- Check kernel configuration
    conf <- kernelConf
    when (lookup "IPC_SHM" conf /= Just "1") $
        sysWrite 1 "SHM not supported" >> sysExit 1

    -- Allocate shared memory
    let size = 1024
    addr <- sysShm size
    case addr of
        Nothing -> sysWrite 1 "SHM allocation failed" >> sysExit 1
        Just _ -> do
            let msg = "Allocated SHM: " ++ show size ++ " bytes"
            sysWrite 1 msg
            sysExit 0
