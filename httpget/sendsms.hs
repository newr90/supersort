-- Test to send a sms via my SMS Gateway ... only for me and the get http showcase
--cabal install network

import Network.HTTP.Simple
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = do
    let url = "http://192.168.50.103/goform/goform_set_cmd_process"

    -- Define headers inside the main function
    let headers = [ ("Origin", "http://192.168.50.103")
                  , ("Referer", "http://192.168.50.103/index.html")
                  , ("Cookie", "Authorization=Basic <your_base64_encoded_password>")
                  ]

    let requestBody = "isTest=false&goformId=SEND_SMS&notCallback=true&Number=%2B491622472546&" <>
                      "sms_time=24%3B05%3B08%3B00%3B23%3B21%3B%2B2&MessageBody=48656c6c6f20576f726c64&" <>
                      "ID=-1&encode_type=GSM7_default"

    -- Create the request object
    let request = setRequestHeaders (map (\(k, v) -> (encodeUtf8 k, encodeUtf8 v)) headers)
                $ setRequestMethod "POST"
                $ setRequestBodyLBS requestBody
                $ parseRequest_ url

    -- Make the request and get the response
    response <- httpLBS request

    -- Print the response status code and body
    putStrLn $ "Response status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ show (getResponseBody response)