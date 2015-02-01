{-# LANGUAGE ScopedTypeVariables #-}

module Utils (
   encode
 , ioLogging
 , makeContext
 , packetLogging
 , tlsParams
 , Context
 , HostName
 , PortNumber
) where

import Data.Default.Class (def)
import qualified Data.ByteString.Lazy as BSL
import qualified Crypto.Random.AESCtr as AESCtr
import qualified Network.IRC as IRC

import BasePrelude
import Network.BSD
import Network.Socket
import Network.TLS
import Network.TLS.Extra.Cipher

ciphers :: [Cipher]
ciphers =
    [ cipher_DHE_RSA_AES256_SHA256
    , cipher_DHE_RSA_AES128_SHA256
    , cipher_DHE_RSA_AES128GCM_SHA256
    , cipher_ECDHE_RSA_AES128GCM_SHA256
    , cipher_AES256_SHA256
    , cipher_AES128_SHA256
    ]

tlsParams :: HostName -> ClientParams
tlsParams host =
  (defaultParamsClient host "")
      { clientSupported = def
           { supportedVersions = [TLS10, TLS12]
           , supportedCiphers = ciphers
           }
      , clientWantSessionResume = Nothing
      , clientUseServerNameIndication = True
      , clientShared = def { sharedValidationCache = vcache }
      }
  where
    vcache =
      ValidationCache (\_ _ _ -> return ValidationCachePass) (\_ _ _ -> return ())

packetLogging :: Logging -> Logging
packetLogging logging =
  logging { loggingPacketSent = putStrLn . ("debug: >> " ++)
          , loggingPacketRecv = putStrLn . ("debug: << " ++)
          }

ioLogging :: Logging -> Logging
ioLogging logging =
  logging { loggingIOSent = putStrLn . ("io: >> " ++) . show
          , loggingIORecv = \hdr -> putStrLn . (("io: << " ++ show hdr ++ " ") ++) . show
          }

makeContext :: HostName -> PortNumber -> IO Context
makeContext hostname port = do
  prg <- AESCtr.makeSystem
  he   <- getHostByName hostname
  sock <- socket AF_INET Stream defaultProtocol
  let sockaddr = SockAddrInet port (head $ hostAddresses he)
  catch (connect sock sockaddr) (\(e :: SomeException) -> error (show e))
  contextNew sock (tlsParams hostname) prg

encode :: IRC.Message -> BSL.ByteString
encode = BSL.fromChunks . return . (<> "\r\n") . IRC.encode
