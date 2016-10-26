{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module UI.Views where

import           Control.Monad               (when)
import           Data.List
import qualified Data.ListLike.String        as L
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                   as T
import           Data.Typed
import           Model.Report
import           React.Flux
import           React.Flux.Addons.Bootstrap
import           UI.Dispatcher
import           UI.Store

updateDelay :: Int
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
           th_ (id_ "chans-head-clients") "Clients"
           th_ (id_ "chans-head-messages") "Values"
      tbody_ (id_ "cl-body") $ mapM_ (uiItem_ . (st,)) $ channels st


uiItem_ (st,chan) = viewWithKey uiItem (channelKey chan) (st,chan) mempty

uiItem :: ReactView (State,Channel)
uiItem = defineView "ui item" $ \(st,chan) -> do
  let tkey = channelKey chan
  tr_ [ --classNames [("completed", uiComplete ui), ("editing", uiIsEditing ui)],
    "key" @= tkey
    ,"id" @= tkey
    ] $ do
    td_ [] $ typeDef_ st chan
    td_ [] $ elemString . unwords . map (show.clientID) . channelClients $ chan
    td_ [] $
      case typesEnv st of
        Nothing -> return ()
        Just _ ->
          case channelInput chan of
            Nothing -> action_ "Show Values" "Watch values transferred on this channel" (OpenChan $ channelType chan)
            Just i  -> do
                action_ "Hide Values" "Stop watching values transferred on this channel" (CloseChan $ channelType chan)
                mapM_ (p_ [] . elemString) $ chanMsgs i

-- typeSource :: State -> Channel -> ReactElementM eventHandler ()
-- typeSource st chan = either (const "") id (maybe  (Left "no types") (\env -> adt_ env <$> typeDefinition env (channelType chan)) (typesEnv st))

typeSource :: State -> Channel -> Either String (ADTEnv,[AbsADT])
typeSource st chan = maybe (Left "no types") (\env -> (env,) <$> typeDefinition env (channelType chan)) (typesEnv st)

-- showADTs :: ADTEnv -> [AbsADT] -> [T.Text]
-- showADTs env = map (prettyText . (env,))
showADTs :: ADTEnv -> [AbsADT] -> [String]
showADTs env = map (prettyShow . (env,))

adt_ :: ADTEnv -> [AbsADT] -> ReactElementM eventHandler ()
adt_ env = span_ [] . mapM_ longTextDisplay_ . showADTs env

-- longTextDisplay_ :: T.Text -> ReactElementM eventHandler ()
longTextDisplay_ :: String -> ReactElementM eventHandler ()
longTextDisplay_ txt = view longTextDisplay txt mempty

longTextDisplay :: ReactView String
longTextDisplay = defineStatefulView "long text" False $ \showAll txt ->
  pre_ [ onClick $ \_ _ st -> ([], Just $ not st)] (elemString $ if showAll then txt else shorter txt)

-- which is faster?, see also canonical and haskell code generation
shorter s =
   let ln = lines s
       o = take 11 ln
   in unlines $ if length ln > 11 then o ++ ["..."] else o

shorterT s =
   let ln = T.lines s
       o = take 11 ln
   in T.unlines $ if length ln > 11 then o ++ ["..."] else o

typeDef_ :: State -> Channel  -> ReactElementM eventHandler ()
typeDef_ st chan = view typeDef (st,chan) mempty

typeDef :: ReactView (State,Channel)
typeDef = defineStatefulView "typeDef" False $ \showDefinition (st,chan) -> do
  h5_ [] $ elemString $ channelShow st chan
  case typeSource st chan of
    Left _ -> return ()
    Right (env,syntax) -> do
       h5_ [onClick $ \_ _ showDef -> ([], Just $ not showDef)
           ,"title" @= unwords ["Click to ",if showDefinition then "hide" else "show","definition"]] (elemText $ T.append "Definition" (if showDefinition then "" else " ..."))
       when showDefinition $ do
          "Copy to Clipboard "
          let canonicalCode = (T.intercalate "\n\n" . map T.pack . showADTs env $ syntax)
          let haskellCode = T.unwords $ ["typed"] ++ (map prettyText . M.keys $ env) ++ ["--srcDir <your_project_src_dir>"]
          copyButton_ haskellCode "Haskell Code"
          " "
          copyButton_ canonicalCode "Canonical Code"
          adt_ env syntax

uiADTs_ :: State -> ReactElementM eventHandler ()
uiADTs_ st = view uiADTs st mempty

uiADTs :: ReactView State
uiADTs = defineStatefulView "all adts" Nothing $ \maybeRef st -> -- do
--    span_ (id_ "all-adts") $ do
     case typesEnv st of
       Nothing -> h4_ [] "Loading data types list ..."
       Just env -> do
         h4_ [] "Known types"
         mapM_ (\(ref,adt) -> span_ ["title" $= "Click to display definition","key" @= prettyShow ref,onClick $ \_ _ _ -> ([],Just (Just ref))] (elemText . T.cons ' ' . declNameT $ adt)) . sortBy (comparing (declNameS . snd)) . M.assocs $ env
         either (const "") (adt_ env) (maybe (Left "no ref") (adtDefinition env) maybeRef)

declNameS = L.toString . declName
declNameT = T.pack . declNameS

-- | A render combinator for the footer
uiFooter_ :: State -> ReactElementM eventHandler ()
uiFooter_ s = view uiFooter s mempty

uiFooter :: ReactView State
uiFooter = defineView "footer" $ \st ->
  footer_ (id_ "footer") . small_ [] $ do
    p_ [] ""
    "Check the "
    a_ ["href" $= "https://github.com/tittoassini/top-apps-ghcjs/blob/master/app/UI/ui.hs"] "source code"
    " of this application."

copyButton_ :: T.Text -> T.Text -> ReactElementM eventHandler ()
copyButton_ txt name = button_ [classNames [("clipboard",True)],"data-clipboard-text" @= txt] $ elemText name

action_ :: T.Text -> String -> Action -> ReactElementM ViewEventHandler ()
action_ name legend act = button_ (["id" @= show act
                                   ,"title" @= legend
                                   ,onClick $ \_ _-> dispatchUI act
                                   ]) $ elemText name

id_ s = [ "id" $= s]

countM n s = strong_ (elemShow n) >> elemText (T.unwords ["",asN n s,""])

-- asN :: Int -> String -> String
asN :: Int -> T.Text -> T.Text
asN n s = T.append s (if n == 1 then "" else "s")

prettyText = T.pack . prettyShow

-- elemString = elemText . T.pack
