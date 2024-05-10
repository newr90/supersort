import Network.HTTP
import Network.HTTP.Types (mkHeader, hContentType)
import Network.URI (parseURI)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Maybe (fromJust)

myRequestURL = "http://192.168.50.103/goform/goform_set_cmd_process"

myRequest :: String -> Request_String
myRequest query = Request { 
    rqURI = fromJust $ parseURI myRequestURL
  , rqMethod = POST
  , rqHeaders = [ mkHeader HdrContentType "text/html"
                , mkHeader HdrContentLength $ show $ length body ]
  , rqBody = body
  }
  where body = "whitepages=" ++ query

main :: IO ()
main = do
  response <- simpleHTTP $ myRequest "poon"