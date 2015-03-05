{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | SendGrid Client

module Email.SendGrid.Client where

import           Data.Bifunctor (bimap)
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method  as NHTM
import qualified Network.HTTP.Types.Status  as NHTS

import           Email.SendGrid.Types

type Reply = Network.HTTP.Client.Response BL8.ByteString

newtype SendGridUser = SendGridUser B.ByteString deriving (Eq, Show)
newtype SendGridKey  = SendGridKey B.ByteString deriving (Eq, Show)

data SendGridCredentials = SendGridCredentials {
    apiUser :: SendGridUser
  , apiKey  :: SendGridKey
  } deriving (Eq, Show)

data SendGridError =
    SendGridUnknownError B.ByteString
  | SendGridWrongCredentials
  deriving (Eq, Show)

data SendGridResponseStatus =
    SendGridSuccess
  | SendGridFailed
  deriving (Eq, Show)

data SendGridResponse = SendGridResponse {
    sgMessage :: SendGridResponseStatus
  , sgErrors  :: [SendGridError]
  } deriving (Eq, Show)

sendGridMailSendEndpoint :: String
sendGridMailSendEndpoint = "https://api.sendgrid.com/api/mail.send.json"

exampleEmail :: [(B.ByteString, B.ByteString)]
exampleEmail = [ ("to[]", "callen.23dc@gmail.com")
               , ("toname[]", "Chris The Coolest")
               , ("to[]", "cma@bitemyapp.com")
               , ("toname[]", "Chris The Cooler")
               , ("subject", "herro, test email")
               , ("text", "SendGrid test email yo :)")
               , ("from", "cma@bitemyapp.com")
                ]

serialiseCredentials :: SendGridCredentials -> [(B.ByteString, B.ByteString)]
serialiseCredentials (SendGridCredentials (SendGridUser user) (SendGridKey key))
  = [ ("api_user", user)
    , ("api_key", key) ]

serialiseEmailAddress :: EmailAddress -> B.ByteString
serialiseEmailAddress (EmailAddress e) = encodeUtf8 e

serialiseRecipientName :: RecipientName -> B.ByteString
serialiseRecipientName (RecipientName r) = encodeUtf8 r

-- fmap (bimap serialiseEmailAddress serialiseRecipientName)

exampleRecipients :: Recipients
exampleRecipients = Recipients
                    [(EmailAddress "callen@woot.com"
                    , RecipientName "Chris Allen")]

tuplesToList :: ((a, b), (a, b)) -> [(a, b)]
tuplesToList ((a, b), (c, d)) = [(a, b), (c, d)]

serialiseEmailName :: B.ByteString
                   -> B.ByteString
                   -> [(EmailAddress, RecipientName)]
                   -> [(B.ByteString, B.ByteString)]
serialiseEmailName e n pairs =
  pairs >>= (tuplesToList . toTuples)
  where toTuples = bimap sEmail sRName
        sEmail = (e,) . serialiseEmailAddress
        sRName = (n,) . serialiseRecipientName

serialiseRecipients :: Recipients -> [(B.ByteString, B.ByteString)]
serialiseRecipients (Recipients addies) =
  serialiseEmailName "to[]" "toname[]" addies

serialiseCc :: CarbonCopies -> [(B.ByteString, B.ByteString)]
serialiseCc (CarbonCopies ccs) =
  serialiseEmailName "cc" "ccname" ccs

serialiseBcc :: BlindCarbonCopies -> [(B.ByteString, B.ByteString)]
serialiseBcc (BlindCarbonCopies bccs) =
  serialiseEmailName "bcc" "bccname" bccs

serialiseFrom :: FromAddress -> [(B.ByteString, B.ByteString)]
serialiseFrom (FromAddress emailAddy) =
  [("from", serialiseEmailAddress emailAddy)]

serialiseSenderName :: SenderName -> [(B.ByteString, B.ByteString)]
serialiseSenderName (SenderName sender) =
  [("fromname", encodeUtf8 sender)]

serialiseEmailBody :: EmailBody -> [(B.ByteString, B.ByteString)]
serialiseEmailBody = undefined

serialiseEmailSubject :: EmailSubject -> [(B.ByteString, B.ByteString)]
serialiseEmailSubject = undefined

serialiseEmail :: Email -> [(B.ByteString, B.ByteString)]
serialiseEmail (Email recipients cc bcc fromAddress
                      senderName emailBody subject)
  = undefined

type SendGridEndpoint = String

sendEmail' :: SendGridEndpoint -> SendGridCredentials -> Email -> IO Reply
sendEmail' url creds email = do
  initReq <- parseUrl url
  let preBody = initReq { method = NHTM.methodPost
                    , checkStatus = \_ _ _ -> Nothing }
      serialisedBody = (serialiseCredentials creds) ++ (serialiseEmail email)
      withBody = urlEncodedBody serialisedBody preBody
  withManager tlsManagerSettings $ httpLbs withBody


sendEmail :: SendGridCredentials -> Email -> IO SendGridResponse
sendEmail creds email = do
  reply <- sendEmail' sendGridMailSendEndpoint creds email
  return $ SendGridResponse SendGridSuccess []
