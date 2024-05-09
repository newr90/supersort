import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let url = "http://192.168.50.103/goform/goform_set_cmd_process"
    let requestBody = "isTest=false&goformId=SEND_SMS&notCallback=true&Number=%2B491622472546&" <>
                      "sms_time=24%3B05%3B08%3B00%3B23%3B21%3B%2B2&MessageBody=48656c6c6f20576f726c64&" <>
                      "ID=-1&encode_type=GSM7_default"

    -- Create a request with the desired headers and body
    let request = setRequestBodyLBS (L8.pack requestBody) $
                  setRequestHeader "Content-Type" (B8.pack "application/x-www-form-urlencoded") $
                  parseRequest_ url

    -- Send the request and get the response
    response <- httpLBS request

    -- Print the response status code and body
    putStrLn $ "Response status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ L8.unpack (getResponseBody response)
