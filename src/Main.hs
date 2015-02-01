module Main where

-- import Data.Default.Class (def)

import qualified Network.IRC as IRC
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Network.TLS as TLS

import BasePrelude hiding (handle)
import Utils

host :: HostName
host = "irc.feelings.blackfriday"

port :: PortNumber
port = 6697

setup :: Context -> IO ()
setup context = do
  let s m = do { print (encode m);  (TLS.sendData context . encode) m }
  s (IRC.Message Nothing "CAP" ["REQ", "multi-prefix", "sasl"])
  s (IRC.Message Nothing "PASS" ["whiskme"])
  s (IRC.nick "thomashauk")
  s (IRC.user "thomashauk" "localhost" "*" "Thomas Hauk")
  s (IRC.Message Nothing "CAP" ["LS"])

main :: IO ()
main = do
  context <- makeContext host port
  -- TLS.contextHookSetLogging context (ioLogging . packetLogging $ def)
  TLS.handshake context
  forkIO $ setup context
  forever (receive context)
  where
    receive context = do
      datum <- TLS.recvData context
      (putStrLn . T.unpack . TE.decodeUtf8) datum
