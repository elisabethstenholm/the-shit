{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- | This module constructs the API request and interprets the response

module Request (
  Command,
  requestBody,
  request,
  suggestions
  ) where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Vector
import Network.HTTP.Req
import Lens.Micro.Platform
import Lens.Micro.Aeson

-- | A console command
type Command = Text

-- | Role used in API request
data Role = 
  System -- ^To set the behavior of the model
  | User -- ^Message for the model to respond to
  deriving (Eq, Ord, Show)

instance ToJSON Role where
  toJSON System = toJSON ("system" :: Text)
  toJSON User = toJSON ("user" :: Text)

-- | Message given to the model
data Message =
  Message {
    role :: Role,
    content :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

-- | GPT model
data Model =
  FourO -- ^4o model
  | FourOMini -- ^4o-mini model
  deriving (Eq, Ord, Show)

instance ToJSON Model where
  toJSON FourO = toJSON ("gpt-4o" :: Text)
  toJSON FourOMini = toJSON ("gpt-4o-mini" :: Text)

-- | Body of API request
data Body =
  Body {
    model :: Model,
    messages :: [Message]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Body where
  toEncoding = genericToEncoding defaultOptions

-- | Messages to model, containing the command to be corrected
requestMessages :: Command -> Text -> [Message]
requestMessages command prevOutput =
  [
    Message {
      role = User,
      content = 
        Text.concat
          [
            "Correct the following terminal command: `",
            command,
            "`. The output of the command was:\n\n```",
            prevOutput,
            "```\n\n",
            "Give me nothing but the command as text, not as a code block."
          ]
      }
  ]

-- | Body of request to model
requestBody :: Command -> Text -> Body
requestBody = (Body FourO .) . requestMessages

-- | API request
request :: (MonadHttp m, ToJSON a, FromJSON b) 
        => a -> ByteString -> m (JsonResponse b)
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
  fmap (read . show) 
  . toListOf (key "choices" . _Array . each . key "message" . key "content" . _String)
