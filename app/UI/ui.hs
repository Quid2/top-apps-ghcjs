module Main where

import           Control.Concurrent
import           Network.Quid2
import           Network.Quid2.Repo (knownTypes)
import           Network.Quid2.Util (seconds)
import           React.Flux
import           System.IO          (stdout)
import           UI.Store
import           UI.Views

main :: IO ()
main = do
  logLevelOut DEBUG stdout
  forkIO $ alterStore store UpdateTypes
  forkIO $ updateLoop
  reactRender "app" uiApp ()
    where updateLoop= alterStore store UpdateChannels >> threadDelay (seconds updateDelay) >> updateLoop
