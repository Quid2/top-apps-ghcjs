module UI.Dispatcher (dispatchUI) where

import React.Flux
import UI.Store

dispatchUI :: Action -> [SomeStoreAction]
dispatchUI a = [SomeStoreAction store a]
