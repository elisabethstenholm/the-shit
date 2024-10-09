{-# LANGUAGE OverloadedStrings #-}

-- | This module constructs the API request and interprets the response
module Request
  ( request
  , suggestions
  ) where

import           Data.Aeson          (Value, object, (.=))
import           Data.ByteString     (ByteString)
import           Lens.Micro.Aeson    (_Array, _String, key)
import           Lens.Micro.Platform (each, toListOf, unpacked)
import           Network.HTTP.Req    (JsonResponse, POST (POST), Req,
                                      ReqBodyJson (ReqBodyJson), https,
                                      jsonResponse, oAuth2Bearer, req, (/:))
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)

requestContent :: String -> String -> String -> String
requestContent command aliases prevCommands =
  printf
    "Given the incorrect terminal command \"%s\", \
    \provide the most likely corrected version of this command. \
    \If there are several highly likely corrections, you may give all of them. \
    \Return only the corrected command(s) formatted as a Haskell list of strings, not inside a code block. \
    \\
    \For reference, I have the following aliases:\n\
    \```%s```\n\
    \and the last commands before the incorrect one were:\n\
    \```%s```\n\
    \in order of execution."
    command
    aliases
    prevCommands

requestBody :: String -> String -> String -> Double -> Value
requestBody command aliases prevCommands temp =
  object
    [ "model" .= ("gpt-4o" :: String)
    , "messages" .=
      [ object
          [ "role" .= ("user" :: String)
          , "content" .= requestContent command aliases prevCommands
          ]
      ]
    , "temperature" .= temp
    ]

-- | API request to model
request ::
     String
  -> String
  -> String
  -> ByteString
  -> Double
  -> Req (JsonResponse Value)
request command aliases prevCommands apikey temp =
  req
    POST
    (https "api.openai.com" /: "v1" /: "chat" /: "completions")
    (ReqBodyJson $ requestBody command aliases prevCommands temp)
    jsonResponse
    (oAuth2Bearer apikey)

-- | Retrieve command suggestions from response
suggestions :: Value -> Maybe [String]
suggestions =
  fmap concat .
  sequence .
  fmap readMaybe .
  toListOf
    (key "choices" .
     _Array . each . key "message" . key "content" . _String . unpacked)
