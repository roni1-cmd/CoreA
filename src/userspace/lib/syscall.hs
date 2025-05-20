-- CoreA System Call Interface (Haskell)
-- Provides system call bindings using FFI

module Syscall where

import Foreign.C
import Foreign.Ptr
import System.Process
import Data.Maybe (fromMaybe)

-- System call numbers
sysWrite :: CInt
sysWrite = 1
sysExit :: CInt
sysExit = 2
sysFork :: CInt
sysFork = 3
sysPipe :: CInt
sysPipe = 4
sysSemaphore :: CInt
sysSemaphore = 5
sysMutex :: CInt
sysMutex = 6
sysShm :: CInt
sysShm = 7

-- Load kernel configuration
kernelConf :: IO [(String, String)]
kernelConf = do
    conf <- readProcess "perl" ["-MData::Dumper", "-e", "print Dumper(do 'config/kernel.conf')"] ""
    return $ read conf -- Assumes config/kernel.conf returns a Perl hash

-- FFI declarations
foreign import ccall unsafe "sys_write" c_sys_write :: CInt -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe "sys_exit" c_sys_exit :: CInt -> IO ()
foreign import ccall unsafe "sys_fork" c_sys_fork :: IO CInt
foreign import ccall unsafe "sys_pipe" c_sys_pipe :: Ptr CInt -> IO CInt
foreign import ccall unsafe "sys_semaphore" c_sys_semaphore :: Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "sys_mutex" c_sys_mutex :: Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "sys_shm" c_sys_shm :: Ptr (Ptr ()) -> CSize -> IO CInt

-- Haskell wrappers
sysWrite :: CInt -> String -> IO CInt
sysWrite fd str = withCString str $ \cstr -> c_sys_write fd cstr (fromIntegral $ length str)

sysExit :: CInt -> IO ()
sysExit = c_sys_exit

sysFork :: IO (Maybe CInt)
sysFork = do
    conf <- kernelConf
    if lookup "PROCESS" conf == Just "1" && lookup "SCHEDULER" conf == Just "ROUND_ROBIN"
        then Just <$> c_sys_fork
        else error "sys_fork not supported: process management disabled"

sysPipe :: IO (Maybe (CInt, CInt))
sysPipe = do
    conf <- kernelConf
    if lookup "IPC_PIPE" conf == Just "1"
        then do
            pipefd <- mallocArray 2 :: IO (Ptr CInt)
            ret <- c_sys_pipe pipefd
            if ret == 0
                then do
                    rfd <- peek pipefd
                    wfd <- peek (pipefd `plusPtr` 4)
                    free pipefd
                    return $ Just (rfd, wfd)
                else do
                    free pipefd
                    return Nothing
        else error "sys_pipe not supported: IPC_PIPE disabled"

sysSemaphore :: CInt -> IO (Maybe CInt)
sysSemaphore value = do
    conf <- kernelConf
    if lookup "IPC_SEMAPHORE" conf == Just "1"
        then do
            semId <- malloc :: IO (Ptr CInt)
            ret <- c_sys_semaphore semId value
            if ret == 0
                then do
                    sid <- peek semId
                    free semId
                    return $ Just sid
                else do
                    free semId
                    return Nothing
        else error "sys_semaphore not supported: IPC_SEMAPHORE disabled"

sysMutex :: CInt -> IO (Maybe CInt)
sysMutex lock = do
    conf <- kernelConf
    if lookup "IPC_MUTEX" conf == Just "1"
        then do
            mutexId <- malloc :: IO (Ptr CInt)
            ret <- c_sys_mutex mutexId lock
            if ret == 0
                then do
                    mid <- peek mutexId
                    free mutexId
                    return $ Just mid
                else do
                    free mutexId
                    return Nothing
        else error "sys_mutex not supported: IPC_MUTEX disabled"

sysShm :: CSize -> IO (Maybe (Ptr ()))
sysShm size = do
    conf <- kernelConf
    if lookup "IPC_SHM" conf == Just "1"
        then do
            addr <- malloc :: IO (Ptr (Ptr ()))
            ret <- c_sys_shm addr size
            if ret == 0
                then do
                    ptr <- peek addr
                    free addr
                    return $ Just ptr
                else do
                    free addr
                    return Nothing
        else error "sys_shm not supported: IPC_SHM disabled"
