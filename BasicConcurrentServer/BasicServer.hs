import Network
import Control.Concurrent
import System.IO

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock

loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       hPutStr h msg
       hFlush h
       hClose h

msg = "HTTP/1.0 200 OK\nContent-Length: 5\n\nPong!\n"
