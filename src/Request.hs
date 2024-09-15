{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module constructs the API request and interprets the response
module Request
  ( requestBody
  , request
  , suggestions
  ) where

import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Lens.Micro.Aeson
import           Lens.Micro.Platform
import           Network.HTTP.Req

-- | Role used in API request
data Role
  = System -- ^To set the behavior of the model
  | User -- ^Message for the model to respond to
  deriving (Eq, Ord, Show)

instance ToJSON Role where
  toJSON System = toJSON ("system" :: Text)
  toJSON User   = toJSON ("user" :: Text)

-- | Message given to the model
data Message =
  Message
    { role    :: Role
    , content :: Text
    }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

-- | GPT model
data Model
  = FourO -- ^4o model
  | FourOMini -- ^4o-mini model
  deriving (Eq, Ord, Show)

instance ToJSON Model where
  toJSON FourO     = toJSON ("gpt-4o" :: Text)
  toJSON FourOMini = toJSON ("gpt-4o-mini" :: Text)

-- | Body of API request
data Body =
  Body
    { model    :: Model
    , messages :: [Message]
    }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Body where
  toEncoding = genericToEncoding defaultOptions

-- | Messages to model, containing the command to be corrected
requestMessages :: Text -> [Message]
requestMessages command =
  [ Message
      { role = User
      , content =
          Text.concat
            [ "Correct the following terminal command: `"
            , command
            , "`. Give me nothing but the command as text, not as a code block."
            ]
      }
  ]

-- | Body of request to model
requestBody :: Text -> Body
requestBody = Body FourO . requestMessages

-- | API request to model
request :: Body -> ByteString -> Req (JsonResponse Value)
request body apikey =
  req
    POST
    (https "api.openai.com" /: "v1" /: "chat" /: "completions")
    (ReqBodyJson body)
    jsonResponse
    (oAuth2Bearer apikey)

-- | Retrieve command suggestions from response
suggestions :: Value -> [String]
suggestions =
  toListOf
    (key "choices" .
     _Array . each . key "message" . key "content" . _String . unpacked)
