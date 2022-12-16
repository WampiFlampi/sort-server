import Control.Monad
import Control.Concurrent
import Control.Exception (bracket)
import Network.Socket.ByteString (recv, sendAll, send)
import Network.Socket
import qualified Data.ByteString.Char8 as C

main :: IO () 
main = do 
    bracket openSock close loop

openSock :: IO Socket
openSock = do 
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrInet 3000 16777343)
      listen sock 1024
      return sock

server :: Socket -> IO ()
server sock = do
  sendAll sock $ C.pack "Input: "
  msg <- recv sock 1024 
  C.putStrLn msg
  if msg /= C.pack "close\n"
  then do
    let mchar = C.unpack $ C.init msg
    let schar = "Sorted: " ++ (msort mchar) ++ "\n\n" 
    sendAll sock $ C.pack schar
    server sock
  else return ()

loop :: Socket -> IO ()
loop sock = do
      conn <- accept sock
      void $ forkFinally (server $ fst conn) (const $ gracefulClose (fst conn) 5000) 
      loop sock


merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) | x == y  = [x] ++ [y] ++ merge xs ys
                        | x < y   = let ls = takeWhile (<y) xs
                                        gs = dropWhile (<y) xs
                                        in [x] ++ ls ++ [y] ++ merge gs ys
                        | x > y   = let ls = takeWhile (<x) ys
                                        gs = dropWhile (<x) ys
                                        in [y] ++ ls ++ [x] ++ merge xs gs
halve :: [a] -> [[a]]
halve [] = []  
halve [x] = [[x]]
halve xs = let len = div (length xs) 2
           in [take len xs, drop len xs] 
   
msort :: Ord a => [a] -> [a] 
msort [x] = [x] 
msort xs = merge (msort (head ls)) (msort (head (tail ls))) 
           where
            ls = halve xs

