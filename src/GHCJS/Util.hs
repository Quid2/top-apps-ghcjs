{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
module GHCJS.Util(getKey) where
import GHCJS.DOM.KeyboardEvent
import GHCJS.Types (JSVal(..), JSString)
import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM.Types

getKey :: (MonadIO m, FromJSString result) => KeyboardEvent -> m result
getKey self = liftIO (fromJSString <$> (js_getKey (self)))
 
foreign import javascript unsafe "$1[\"key\"]" js_getKey :: KeyboardEvent -> IO JSString

