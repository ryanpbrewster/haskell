import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
 
type Msg = (Int, String)
 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hi, what's your name?"
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl line
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case line of
         "quit" -> hPutStrLn hdl "Bye!"
         _      -> do
            broadcast (name ++ ": " ++ line)
            loop
    killThread reader
    broadcast ("<-- " ++ name ++ " left.")
    hClose hdl
