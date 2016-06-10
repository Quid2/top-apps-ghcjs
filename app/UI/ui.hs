module Main where

import           Control.Concurrent
import           Network.Top
import           Network.Top.Repo (knownTypes)
import           Network.Top.Util (seconds)
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
    where updateLoop = alterStore store UpdateChannels >> threadDelay (seconds updateDelay) >> updateLoop
