{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

-- |A very basic chat client
module Main (
  main
  ) where
import           Chat.Model
import           Chat.Util
import           Control.Applicative        ((<$>))
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
-- import           Data.Digest.SHA3
import qualified Data.JSString              as S
-- import           Data.Pattern
import           Data.Word
import           GHCJS.DOM                  (enableInspector, runWebGUI,
                                             webViewGetDomDocument)
import           GHCJS.DOM.Document         hiding (focus, keyPress)
import           GHCJS.DOM.Element          hiding (click)
import           GHCJS.DOM.EventM           (event, mouseClientXY, on)
import           GHCJS.DOM.HTMLInputElement
import           GHCJS.DOM.KeyboardEvent
import           GHCJS.DOM.Node
import           GHCJS.DOM.UIEvent
import           GHCJS.Marshal
import           GHCJS.Util                 (getKey)
-- import           JavaScript.Web.WebSocket
import           Network.Top                hiding (cat)
import           Pipes                      (Producer', cat, for)
import           Pipes.Concurrent           hiding (send)
import qualified Pipes.Concurrent           as PC
import           Pipes.Prelude              (stdinLn)
import           Pipes.Safe
import           System.Environment
import           System.IO                  (stdout)

main = runWebGUI $ \ webView -> do
     -- enableInspector webView
     dbgS "started"
     logLevel DEBUG

     Just doc <- webViewGetDomDocument webView
     Just body <- getBody doc
     setInnerHTML body (Just initialHtml)
     Just inp <- getElementById doc "ipt"
     Just out <- getElementById doc "out"
     Just status <- getElementById doc "status"
     dbgS "here 1"
     let
       -- The subject of the discussion
       subjL = ["Haskell"]
       subj = Subject subjL

       addMsg = addTxt doc out -- addPar doc out
       addStatus = addPar doc status

     Just user <- runSafeT $ execStateT
         (runEffect
           (userInput doc inp "Enter your name (at least 3 characters long)" 3 20 >->
           (do
               userName <- await
               liftIO $ addStatus (unwords ["Current Subject:",prettySubject (Subject []) subj,"\n"])
               liftIO $ addStatus ("Connected as: "++userName)
               lift $ put $ Just userName
               return ()))) Nothing

     -- let user = "tst"
     -- Asynchronously, receive messages and display them
     -- We use a simple pipe, to get a message from the connection (pipeIn) and print it
     async $ runConn ByType $ \conn -> void $ execStateT (runEffect (pipeIn conn >-> niceMessage subj >-> for cat (void . liftIO . addMsg))) (ChatState False)

     runConn ByType $ \conn -> do

       let msg = Message (User user) subj
           outputOne = output conn . msg
           ping = threadDelay (seconds 30) >> outputOne Ping >> ping

       -- ?Wait a bit to give the message receiving pipe a chance to get ahead
       threadDelay (milliseconds 500)

       -- Let everybody know that we joined the discussion
       outputOne Join

       -- Then ask for recent messages
       outputOne AskHistory

       -- Read lines from the user (stdinLn) and output them out (pipeOut)
       runPipe $ for (userInput doc inp "Enter your message" 1 120) (\txt -> unless (null txt) (yield . msg . TextMessage $ txt)) >-> pipeOut conn

       --async ping

-- userInput :: Document -> Element -> String -> Int -> Int -> Producer' String (SafeT IO) ()
userInput doc out title minLen maxLen = bracket setup final run
  where
    setup = do
      setInnerHTML out $ Just (concat ["<input type=text maxlength=",show maxLen," size=",show (max 60 maxLen)," id='inputArea' placeholder='",title,"'></input>"])
      Just inputArea <- getElementById doc "inputArea"
      inElem <- castToHTMLInputElement inputArea
      focus inElem
      (output, input, seal) <- PC.spawn' PC.Unbounded
      on inElem keyPress $ do
        e <- event
        liftIO $ do
          ki <- getCharCode e
          ki2 <- getKeyCode e
          ki3 <- getWhich e
          --dbg ["char code",show ki,"key code",show ki2,"which",show ki3]
          when (ki2 == 13) $ do
            Just val <- getValue inElem :: IO (Maybe String)
            when (length val >= minLen) $ do
              atomically $ PC.send output val
              setValue inElem (Just "")
      return (input,atomically seal)

      -- run :: (Input String,IO ()) -> (Producer String (SafeT IO) ())
    run (input,_) = fromInput input

    final (_,seal) = do
      seal
      setInnerHTML out (Just "")

runConn = runClient def -- (def {ip="127.0.0.1"})

runPipe = runSafeT . runEffect

addPar doc to txt = do
   Just newParagraph <- createElement doc (Just "pre")
   text <- createTextNode doc txt
   appendChild newParagraph text
   appendChild to (Just newParagraph)
   return newParagraph

addTxt doc to txt = do
   text <- createTextNode doc (txt++"\n")
   getFirstChild to >>= insertBefore to text

initialHtml :: String
initialHtml = "<h1>Chat</h1><p id='status'/><p id='ipt'/><pre id='out'/>"

