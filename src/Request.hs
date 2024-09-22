{-# LANGUAGE OverloadedStrings #-}

-- | This module constructs the API request and interprets the response
module Request
  ( requestBody
  , request
  , suggestions
  ) where

import           Data.Aeson          (Value, object, (.=))
import           Data.ByteString     (ByteString)
import           Data.Text.Format    (format)
import           Data.Text.Lazy      (Text)
import           Lens.Micro.Aeson    (_Array, _String, key)
import           Lens.Micro.Platform (each, toListOf, unpacked)
import           Network.HTTP.Req    (JsonResponse, POST (POST), Req,
                                      ReqBodyJson (ReqBodyJson), https,
                                      jsonResponse, oAuth2Bearer, req, (/:))

requestContent :: String -> Text
requestContent command =
  format
    "Given the incorrect terminal command \"{}\",\
    \ provide the most likely corrected version of this command.\
    \ If there are several highly likely corrections, you may give all of them.\
    \ Return only the corrected command(s) formatted as a Haskell list of strings, not inside a code block."
    [command]

requestBody :: String -> Value
requestBody command =
  object
    [ "model" .= ("gpt-4o" :: String)
    , "messages" .=
      [ object
          ["role" .= ("user" :: String), "content" .= requestContent command]
      ]
    , "temperature" .= (1 :: Double)
    ]

-- | API request to model
request :: String -> ByteString -> Req (JsonResponse Value)
request command apikey =
  req
    POST
    (https "api.openai.com" /: "v1" /: "chat" /: "completions")
    (ReqBodyJson $ requestBody command)
    jsonResponse
    (oAuth2Bearer apikey)

-- | Retrieve command suggestions from response
suggestions :: Value -> [String]
suggestions =
  concat .
  (read <$>) .
  toListOf
    (key "choices" .
     _Array . each . key "message" . key "content" . _String . unpacked)
