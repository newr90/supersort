import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let url = "http://192.168.50.11:5000/webhook"
    let requestBody = ""

    request <- parseRequest $ "POST " ++ url
    let request' = setRequestBodyLBS (L8.pack requestBody) request
    response <- httpLBS request'

    putStrLn $ "Response status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ L8.unpack (getResponseBody response)