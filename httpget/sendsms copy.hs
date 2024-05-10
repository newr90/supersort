{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types.Header (Header, hContentType)

-- Function to build a request with headers
buildRequest :: String -> [Header] -> RequestBody -> IO Request
buildRequest url headers body = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "POST", requestHeaders = headers, requestBody = body })

-- Function to send a request with headers and get the response
send :: [Header] -> RequestBody -> IO ()
send headers requestBody = do
  manager <- newManager defaultManagerSettings
  request <- buildRequest "http://httpbin.org/post" headers requestBody
  response <- httpLbs request manager
  let Just obj = decode (responseBody response)
  print (obj :: Object)

main :: IO ()
main = do
  -- Example headers
  let headers = [(hContentType, "application/json")]
  -- Example request body
  let requestBody = RequestBodyLBS "{\"key\": \"value\"}"
  -- Send request with headers and body
  send headers requestBody
