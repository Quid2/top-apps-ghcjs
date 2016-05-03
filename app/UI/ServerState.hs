 {-# LANGUAGE OverloadedStrings           #-}
module UI.ServerState(getByType,getServerState) where
import Data.Typed
import JavaScript.Web.XMLHttpRequest
import qualified Data.JSString as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Report
import Control.Monad
import Data.Maybe
import Network.Quid2.Types(Config(..),def)

getByType :: IO [(AbsType, ClientReport)]
getByType = fmap byTypeReport (getServerState def)

getServerState cfg = do
  res <- xhrByteString $ Request {reqMethod=GET
                                 ,reqURI= J.pack $ concat["http://",ip cfg,":",show (port cfg),"/report"]
                                 ,reqLogin=Nothing
                                 ,reqHeaders=[]
                                 ,reqWithCredentials=False
                                 ,reqData=NoData}
  let Just dt = contents res
  print $ B.unpack dt
  let Right report = unflat . L.fromStrict $ dt
  printReport report
  return report

byTypeReport st = let [ByTypeReport vs] = byTypeReport_ st
                  in vs

byTypeReport_ (NestedReport n (TypedBytes t bs) ss) =
   if (t == byTypeReportType)
   then [dec bs::ByTypeReport]
   else concatMap byTypeReport_ ss

printReport (NestedReport n (TypedBytes t bs) ss) = do
  putStrLn n
  print t
  print ("warpReportType",warpReportType)
  when (t == warpReportType) $ print (dec bs::WarpReport)
  when (t == byTypeReportType) $ print (dec bs::ByTypeReport)
  when (t == echoReportType) $ print (dec bs::[ClientReport])
  mapM_ printReport ss

dec bs = let Right a = unflat (L.pack bs) in a

warpReportType = absType (Proxy::Proxy WarpReport)
stringType = absType (Proxy::Proxy String)
intType = absType (Proxy::Proxy Int)
byTypeReportType = absType (Proxy::Proxy ByTypeReport)
echoReportType = absType (Proxy::Proxy [ClientReport])

