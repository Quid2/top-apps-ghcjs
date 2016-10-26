{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass ,StandaloneDeriving, FlexibleInstances ,TypeSynonymInstances#-}
module UI.Store where
import Model.Report
import React.Flux
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Typed
import UI.ServerState
import Control.Concurrent
import Control.Monad
import Data.List
import Network.Top
import qualified Data.Map as M
import Data.Ord
import Data.Maybe

data State = State {channels::[Channel]
                   ,typesEnv::Maybe ADTEnv
                   } deriving (Show,Typeable,Generic,NFData)

data Channel = Channel {channelType::AbsType
                       ,channelClients::[ClientReport]
                       ,channelInput::Maybe ChannelInput
                       }
    deriving (Show, Typeable,Generic,NFData)

data ChannelInput = ChannelInput {chanThread::ThreadId
                                 ,chanDecoder::TypeDecoders
                                 ,chanMsgs::[String]}
                  deriving (Show, Typeable,Generic,NFData)

channelKey :: Channel -> String
channelKey = prettyShow . channelType

channelShow :: State -> Channel -> String
channelShow st chan = maybe ("Unknown type with unique code: "++channelKey chan) (\env -> prettyShow (label env declName (channelType chan))) $ typesEnv st

data Action = UpdateChannels
            | UpdateTypes
            | SetEnv ADTEnv
            | SetChans [Channel]
            | SetChannelInput AbsType (Maybe ChannelInput)
            | OpenChan AbsType
            | CloseChan AbsType
            | MsgIn AbsType ByteString
            | TabChange Int
  deriving (Show, Typeable, Generic, NFData)

instance StoreData State where
    type StoreAction State = Action
    transform action st = do
        dbg ["Action:",take 500 $ show action]
        --dbg ["Initial state:",show st]

        st' <- case action of

          UpdateTypes -> do
            forkIO $ do
              eenv <- knownTypes def
              case eenv of
                Left e    -> do
                  dbg ["UpdateTypes problem",e]
                  threadDelay (seconds 60)
                  alterStore store UpdateTypes
                Right env -> alterStore store (SetEnv (M.fromList env))
            return st

          UpdateChannels -> do
            forkIO $ do
              chans <- getByType
              let cs = map (\l -> (fst . head $ l,map snd l)) . groupBy (\a b -> fst a == fst b) . sort $ chans
              -- alterStore store (SetChans (resolve st $ merge cs (channels st)))
              alterStore store (SetChans (merge cs (channels st)))
            return st

          OpenChan t -> do
            let openChan env chan = when (isNothing $ channelInput chan) $ do
                 tid <- forkIO $ runClient_ def (byTypeRouter t) rcvMessages
                 void $ forkIO $ do
                   let i = ChannelInput tid (typeDecoderEnv env t) []
                   alterStore store (SetChannelInput t (Just i))
            cond (openChan <$> typesEnv st <*> lookChan st t)
            return st
              where rcvMessages conn = do
                      r <- input conn
                      alterStore store (MsgIn t r)
                      rcvMessages conn

          CloseChan t -> do
             let closeChan i = void . forkIO $ do
                   killThread $ chanThread i
                   alterStore store (SetChannelInput t Nothing)
             cond $ closeChan <$> (lookChan st t >>= channelInput)
             return st

          MsgIn t msg -> modifyChan st t (\ch -> ch {channelInput = (\i-> i {chanMsgs = (prettyShow . runGet (typeDecoder (chanDecoder i) t) $ msg):chanMsgs i}) <$> channelInput ch})

          SetEnv env -> return $ st {typesEnv=Just env}

          SetChans chans -> return $ st {channels=chans}

          SetChannelInput t i -> modifyChan st t (\ch -> ch {channelInput=i})

          _ -> return st

        -- dbg ["New state:",show st']
        return st'
          where
            cond = fromMaybe (return ())

            onChan st t f = case lookChan st t of
               Nothing -> return st
               Just chan -> f chan

            lookChan st t = find ((t ==) . channelType) (channels st)

            modifyChan st t f = return $ st {channels = map (\ch -> if t == channelType ch then f ch else ch) (channels st)}

-- on mv = case mv of Nothing -> return 

merge ns os = reverse $ mer [] ns os
  where
    mer l [] os = l
    mer l ns [] = reverse (map newChan ns) ++ l
    mer l ns@(nh@(nt,ncs):nn) os@(oh:oo) | nt < channelType oh  = mer (newChan nh : l) nn os
                                         | nt == channelType oh = mer (oh {channelClients=ncs} : l) nn oo
                                         | otherwise = mer l ns oo

    newChan (nt,ncs) = Channel nt ncs Nothing

store :: ReactStore State
store = mkStore $ State [] Nothing

-- TODO: move in library
-- deriving instance NFData State
-- deriving instance NFData Channel
deriving instance NFData Time
-- deriving instance NFData ADTRef
-- deriving instance (NFData a,NFData b) => NFData (ADT a b) 
-- deriving instance NFData a => NFData (Type a)
-- deriving instance NFData a => NFData (ConTree a)
-- deriving instance NFData a => NFData (Ref a)
-- deriving instance NFData a => NFData (NonEmptyList a)
deriving instance NFData ClientReport

