{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module UI.Views where

import           Control.Monad               (when)
import           Data.List
import qualified Data.Map                    as M
import           Data.Ord
import qualified Data.Text                   as T
import           Data.Typed
import           React.Flux
import           React.Flux.Addons.Bootstrap
import           Report
import           UI.Dispatcher
import           UI.Store

updateDelay = 15

-- | The controller view and also the top level of the UI app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
uiApp :: ReactView ()
uiApp = defineControllerView "ui app" store $ \st () ->
    div_ $ do
        -- uiHeader_
        bootstrap_ "Tabs" ["id" $= "main-tabs","defaultActiveKey" @= (1 :: Int)
                   , callback "onSelect" $ \(i :: Int) -> dispatchUI $ TabChange i
                   ] $ do
          bootstrap_ "Tab" ["eventKey" @= (1 :: Int),"title" $= "Channels"] $ uiChannels_ st
          bootstrap_ "Tab" ["eventKey" @= (2 :: Int),"title" $= "Types"] $ uiADTs_ st
        uiFooter_ st

-- | The UI header as a React view with no properties.
uiHeader :: ReactView ()
uiHeader = defineView "header" $ \() ->
  header_ ["id" $= "header"] $ do
        h1_ "Channels"
        -- bootstrap_ "Alert" [ "bsStyle" $= "danger"
        --             -- , callback "onDismiss" $ dispatch CloseAlert
        --             ] $
        --   p_ "Hello, World!"

-- | A combinator for the header suitable for use inside the 'uiApp' rendering function.
uiHeader_ :: ReactElementM eventHandler ()
uiHeader_ = view uiHeader () mempty

uiChannels_ :: State -> ReactElementM ViewEventHandler ()
uiChannels_ st = section_ (id_ "main") $
  let n = length $ channels st
      nc = sum . map (length . channelClients) $ channels st
  in do
     -- labeledInput_ "toggle-all" "Mark all as complete"
     --    [ "type" $= "checkbox"
     --    , "checked" $= if all (uiComplete . snd) $ uiList st then "checked" else ""
     --    , onChange $ \_ -> dispatchUi ToggleAllComplete
     --    ]
    p_ (id_ "actions") $ do
       action_ "Update Channels" (unwords ["The list of open channels is automatically updated every",show updateDelay,"secs, click here to update immediately"]) UpdateChannels
       span_ (id_ "sep") "   "
       action_ "Update Types" "Update the list of known types" UpdateTypes

    p_ (id_ "count") $ do
       countM n "channel"
       countM nc "client"

    table_ (id_ "chans-list") $ do
      thead_ (id_ "cl_head") $ do
        tr_ (id_ "cl-header") $ do
           th_ (id_ "chans-head-type") "Type"
           th_ (id_ "chans-head-definition") "Definition"
           th_ (id_ "chans-head-clients") "Clients"
           th_ (id_ "chans-head-messages") "Values"
      tbody_ (id_ "cl-body") $ mapM_ (uiItem_ . (st,)) $ channels st


uiItem_ (st,chan) = viewWithKey uiItem (channelKey chan) (st,chan) mempty

-- | A view for each ui item.  We specifically use a ReactView here to take advantage of the
-- ability for React to only re-render the ui items that have changed.  Care is taken in the
-- transform function of the store to not change the Haskell object for the pair (Int, Ui), and
-- in this case React will not re-render the ui item.  For more details, see the "Performance"
-- section of the React.Flux documentation.
uiItem :: ReactView (State,Channel)
uiItem = defineView "ui item" $ \(st,chan) -> do
  let tkey = channelKey chan
  tr_ [ --classNames [("completed", uiComplete ui), ("editing", uiIsEditing ui)],
     "key" @= tkey
    ,"id" @= tkey
    ] $ do

        -- cldiv_ "view" $ do
        --     input_ [ "className" $= "toggle"
        --            , "type" $= "checkbox"
        --            , "checked" @= uiComplete ui
        --            , onChange $ \_ -> dispatchUi $ UiSetComplete uiIdx $ not $ uiComplete ui
        --            ]

             td_ [] $ elemText $ channelShow st chan
             td_ [] $ typeSource st chan
             td_ [] $ elemText . unwords . map (show.clientID) . channelClients $ chan
             td_ [] $
               case typesEnv st of
                 Nothing -> return ()
                 Just _ ->
                   case channelInput chan of
                     Nothing -> action_ "Show Values" "Watch values transferred on this channel" (OpenChan $ channelType chan)
                     Just i  -> do
                       action_ "Hide Values" "Stop watching values transferred on this channel" (CloseChan $ channelType chan)
                       mapM_ (p_ [] . elemText) $ chanMsgs i

        -- when (uiIsEditing ui) $
        --     uiTextInput_ TextInputArgs
        --         { tiaId = Nothing
        --         , tiaClass = "edit"
        --         , tiaPlaceholder = ""
        --         , tiaOnSave = dispatchUi . UpdateText uiIdx
        --         , tiaValue = Just $ uiText ui
        --         }

typeSource :: State -> Channel -> ReactElementM eventHandler ()
typeSource st chan = maybe "" (\env -> adt_ env $ typeDefinition env (channelType chan)) $ typesEnv st

adt_ :: ADTEnv -> [AbsADT] -> ReactElementM eventHandler ()
adt_ env = span_ [] . mapM_ (longTextDisplay_ . prettyShow . (env,))

longTextDisplay_ :: String -> ReactElementM eventHandler ()
longTextDisplay_ txt = view longTextDisplay txt mempty

longTextDisplay :: ReactView String
longTextDisplay = defineStatefulView "long text" False $ \showAll txt ->
     pre_ [ onClick $ \_ _ st -> ([], Just $ not st)] (elemText $ if showAll then txt else shorter txt)

shorter s =
   let ln = lines s
       o = take 11 ln
   in unlines $ if length ln > 11 then o ++ ["..."] else o

uiADTs_ :: State -> ReactElementM eventHandler ()
uiADTs_ st = view uiADTs st mempty

uiADTs :: ReactView State
uiADTs = defineStatefulView "all adts" Nothing $ \maybeRef st -> do
  p_ (id_ "all-adts") $ do
    case typesEnv st of
      Nothing -> "Loading data types list ..."
      Just env -> do
        h4_ [] "Known types"
        mapM_ (\(ref,adt) -> span_ ["title" $= "Click to display definition","key" @= prettyShow ref,onClick $ \_ _ _ -> ([],Just (Just ref))] (elemText . (' ':) . declName $ adt)) . sortBy (comparing (declName . snd)) . M.assocs $ env
        maybe (elemText "") (adt_ env . adtDefinition env) maybeRef

-- | A render combinator for the footer
uiFooter_ :: State -> ReactElementM eventHandler ()
uiFooter_ s = view uiFooter s mempty

uiFooter :: ReactView State
uiFooter = defineView "footer" $ \st ->
  footer_ (id_ "footer") $ ""

action_ :: String -> String -> Action -> ReactElementM ViewEventHandler ()
action_ name legend act = button_ (["id" @= show act
                                   ,"title" @= legend
                                   ,onClick $ \_ _-> dispatchUI act
                                   ]) $ elemText name

id_ s = [ "id" $= s]

countM n s = strong_ (elemShow n) >> elemText (unwords ["",asN n s,""])

asN :: Int -> String -> String
asN n s = s ++ (if n == 1 then "" else "s")



