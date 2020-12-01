{-# LANGUAGE UnicodeSyntax #-}

module NetworkMain
  ( Msg
  , ConFunc
  , toBS
  , fromBS
  , networkMain
  , NetworkMain.send
  , recvLine
  , recvEnough
  , recvRaw
  , close
  ) where

import           Control.Concurrent
import           Control.Monad                  (void, when)
import           Control.Monad.Fix              (fix)
import           Data.Char                      (ord)
import           Data.Maybe                     (fromJust, isJust, isNothing)
import           GHC.Int                        (Int64)
import           Network.Socket
import           Network.Socket.ByteString.Lazy as NSBL

-- for working with ByteString
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy.Char8     as BLC
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TLE

type Msg = (ByteString, Int)

type ConFunc = (Socket, SockAddr) → Chan Msg → Int → IO ()

toBS ∷ String → ByteString
toBS = TLE.encodeUtf8 . TL.pack

fromBS ∷ ByteString → String
fromBS = TL.unpack . TLE.decodeUtf8

nullToNothing ∷ ByteString → Maybe ByteString
nullToNothing bs
  | BLC.null bs = Nothing
  | otherwise = Just bs

networkMain ∷ PortNumber → ConFunc → Bool → IO ()
networkMain port conFunc echoMsgs = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port $ tupleToHostAddress (0, 0, 0, 0))
  listen sock 5
  chan <- newChan
  when echoMsgs $
    void $
    forkIO $
    fix $ \loop -> do
      (msg, _) <- readChan chan
      putStr $ fromBS msg
      loop
  mainLoop sock chan 0 conFunc

mainLoop ∷ Socket → Chan Msg → Int → ConFunc → IO ()
mainLoop sock chan clientID conFunc = do
  con <- accept sock
  void $ forkIO (conFunc con chan clientID)
  (mainLoop sock chan $! clientID + 1) conFunc

send ∷ Socket → ByteString → IO Bool
send sock msg = do
  let len = BLC.length msg
  sent <- NSBL.send sock msg
  if sent == 0
    then return False -- connection lost
    else if sent < len -- send leftover data
           then NetworkMain.send sock (BLC.drop sent msg)
           else return True -- finished

recvLine ∷ Socket → IO (Maybe ByteString)
recvLine sock = do
  first <- fmap BLC.head . nullToNothing <$> recv sock 1 -- try to get a byte
  if isNothing first -- connection lost
    then return Nothing
    else if fromJust first == '\n' -- end of line
           then return $ Just $ toBS ""
          -- continue receiving
           else do
             next <- recvLine sock
             if isJust next -- success, append byte to new data
               then return $ Just $ BLC.cons (fromJust first) (fromJust next)
         -- connection lost
               else return Nothing

recvEnough ∷ Socket → GHC.Int.Int64 → IO (Maybe ByteString)
recvEnough sock len = do
  someData <- recv sock len
  let thisLen = BLC.length someData
  if thisLen == 0 -- connection lost
    then return Nothing
    else if thisLen == len -- all data received
           then return $ Just someData
          -- receive more data
           else do
             moreData <- recvEnough sock (len - thisLen)
             if isNothing moreData -- connection lost before all data could be received
               then return Nothing
         -- all data received, build final value
               else return $ Just $ BLC.append someData (fromJust moreData)

recvRaw ∷ Socket → IO (Maybe ByteString)
recvRaw sock
  -- try to get a byte
 = do
  len <- fmap (ord . BLC.head) . nullToNothing <$> recv sock 1
  if isNothing len
    then return Nothing
    -- try to receive
    else recvEnough sock (toEnum $ fromJust len)
