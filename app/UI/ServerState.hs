module UI.ServerState where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.JSString                 as J
import           JavaScript.Web.XMLHttpRequest
import           Model.Report
import           Model.Report.Util
import Data.Typed
import Network.Top.Types(def)

-- getChannels :: IO [(AbsType, ClientReport)]
getChannels = do
  st <- getServerState def
  let c1 = byTypeReport st
  let c2 = map (\(t,p,c) -> (t,c)) $ byPatternReport st
  return $ c1 ++ c2

-- getByType :: IO [(AbsType, ClientReport)]
-- getByType = fmap byTypeReport (getServerState def)

getServerState cfg = do
  res <- xhrByteString $ Request {reqMethod=GET
                                 ,reqURI= J.pack $ reportURL cfg
                                 ,reqLogin=Nothing
                                 ,reqHeaders=[]
                                 ,reqWithCredentials=False
                                 ,reqData=NoData}
  let Just dt = contents res
  print $ B.unpack dt
  let Right report = unflat . L.fromStrict $ dt
  printReport report
  return report
