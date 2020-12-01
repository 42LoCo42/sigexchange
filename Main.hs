{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Concurrent
import           Control.Monad              (void, when)
import           Control.Monad.Fix          (fix)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe                 (fromJust, isJust)
import           NetworkMain

main :: IO ()
main = networkMain 37812 runCon True

runCon :: ConFunc
runCon (sock, _) chan clientID = do
  let brodcast msg = writeChan chan (msg, clientID)
  -- Get name from client
  void $ send sock $ toBS "Hi, what's your name?\n"
  maybeName <- recvLine sock
  if isJust maybeName
    then do
      let name = fromJust maybeName
      let nameS = fromBS name
      brodcast $ toBS (nameS ++ " has connected!\n")
      void $ send sock $ toBS ("Welcome, " ++ nameS ++ "!\n")
    -- Read from other clients
      commLine <- dupChan chan
      reader <-
        forkIO $
        fix $ \loop -> do
          (line, newID) <- readChan commLine
          when (newID /= clientID) $ void (send sock line)
          loop
    -- Read from our client
      fix $ \loop -> do
        maybeLine <- recvLine sock
        if isJust maybeLine
          then do
            writeChan
              chan
              ( BLC.concat
                  [name, toBS ": ", fromJust maybeLine, BLC.singleton '\n']
              , clientID)
            loop
          else do
            killThread reader
            brodcast $ toBS (nameS ++ " has disconnected!\n")
            close sock
    else close sock -- error on name transfer
