{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Prelude hiding (takeWhile)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Applicative ( (<|>) )
import Data.Default (Default(def))
import Data.IP (toIPv4)
import Data.Maybe
import System.Environment (getArgs)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

data Conf = Conf
  { bufSize     :: Int
  , timeOut     :: Int
  , nameservers :: [HostName]
  }

instance Default Conf where
    def = Conf
      { bufSize     = 512
      , timeOut     = 10 * 1000 * 1000
      , nameservers = []
      }

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

{--
 - Proxy dns request to a real dns server.
 -}
proxyRequest :: Conf -> HostName -> DNSFormat -> IO (Either String DNSFormat)
proxyRequest Conf{..} server req = do
    let rc = defaultResolvConf { resolvInfo = RCHostName server }
        worker Resolver{..} = do
            let packet = B.concat . BL.toChunks $ encode req
            sendAll dnsSock packet
            receive dnsSock
    rs <- makeResolvSeed rc
    withResolver rs $ \r ->
        (>>= check) . toEither "proxy request timeout" <$> timeout timeOut (worker r)
  where
    ident = identifier . header $ req
    check :: DNSFormat -> Either String DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Right rsp
                        else Left "identifier not match"

{--
 - Parse out EC2-style "hostnames" derived from the IP address, e.g.
 - ec2-203-0-113-25.compute-1.amazonaws.com. -> 203.0.113.25
 -}
resolveEc2Hostname :: DNSMessage -> Maybe DNSMessage
resolveEc2Hostname req@(DNSMessage {question = (q@(Question domain A):_)}) =
    maybeResult $ parse parseEc2 domain
  where
    ident = identifier . header $ req
    parseEc2 :: Parser DNSMessage
    parseEc2 = do
        _ <- "ec2-" <|> "ip-"
        octets <- map read <$> (many1 digit `sepBy` string "-")
        case length octets of
          4 ->
              return $ responseA ident q [ip]
            where ip = toIPv4 octets
          _ -> fail "not an ec2 ip"

resolveEc2Hostname _ = Nothing

{--
 - Handle A query for domain suffixes configured, and proxy other requests to real dns server.
 -}
handleRequest :: Conf -> DNSFormat -> IO (Either String DNSFormat)
handleRequest conf req =
    case resolveEc2Hostname req of
        (Just rsp) -> return $ Right rsp
        Nothing  -> maybe
                    (return $ Left "nameserver not configured.")
                    (\srv -> proxyRequest conf srv req)
                    (listToMaybe (nameservers conf))

{--
 - Parse request and compose response.
 -}
handlePacket :: Conf -> Socket -> SockAddr -> B.ByteString -> IO ()
handlePacket conf@Conf{..} sock addr s =
    either
    (putStrLn . ("decode fail:"++))
    (\req -> do
        -- putStrLn $ show req
        handleRequest conf req >>=
            either
            putStrLn
            (\rsp -> let packet = B.concat . BL.toChunks $ encode rsp
                     in  timeout timeOut (sendAllTo sock packet addr) >>=
                         maybe (putStrLn "send response timeout") return
            )
    )
    (decode (BL.fromChunks [s]))

run :: Conf -> IO ()
run conf = withSocketsDo $ do
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
    bindSocket sock (addrAddress addrinfo)
    forever $ do
        (s, addr) <- recvFrom sock (bufSize conf)
        forkIO $ handlePacket conf sock addr s

{--
 - parse config file.
 -}
readHosts :: FilePath -> IO [HostName]
readHosts filename =
    B.readFile filename >>= either (fail . ("parse hosts fail:"++)) return . parseHosts
  where
    parseHosts :: B.ByteString -> Either String [HostName]
    parseHosts s = mapM (parseOnly nameserver) $ B.lines s
    nameserver :: Parser HostName
    nameserver = do
        _ <- string "nameserver"
        _ <- space
        skipSpace
        B.unpack <$> takeWhile (not . isSpace)

main :: IO ()
main = do
    args <- getArgs
    servers <- readHosts $ fromMaybe "./hosts" (listToMaybe args)
    print servers
    run def{nameservers=servers}
