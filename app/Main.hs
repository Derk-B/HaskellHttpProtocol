{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        let (_: msg') = show msg
        print msg
        printLines msg' ""
        unless (S.null msg) $ do
          sendAll s msg
          talk s

printLines :: String -> String -> IO ()
printLines ['\"'] s = return ()
printLines ('\\':'r':'\\':'n':xs) s = do 
    putStrLn s
    printLines xs ""
printLines (s:xs) str = printLines xs (str ++ [s])

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)