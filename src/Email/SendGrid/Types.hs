-- | Types for talking to Sendgrid 

module Email.SendGrid.Types where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

newtype EmailAddress  = EmailAddress  T.Text      deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text      deriving (Eq, Show)
newtype Recipients    = Recipients
                        [(EmailAddress, RecipientName)] deriving (Eq, Show)

newtype CarbonCopies = CarbonCopies
                       [(EmailAddress, RecipientName)] deriving (Eq, Show)

newtype FromAddress     = FromAddress      EmailAddress deriving (Eq, Show)
newtype SenderName      = SenderName       T.Text       deriving (Eq, Show)

newtype EmailTextBody     = EmailTextBody     T.Text      deriving (Eq, Show)
newtype EmailHtmlBody     = EmailHtmlBody     T.Text      deriving (Eq, Show)
newtype EmailSubject      = EmailSubject      T.Text      deriving (Eq, Show)
newtype BlindCarbonCopies = BlindCarbonCopies [(EmailAddress, RecipientName)]     deriving (Eq, Show)

data EmailBody = HtmlAndText EmailTextBody EmailHtmlBody
               | JustText    EmailTextBody
               | JustHtml    EmailHtmlBody
                 deriving (Eq, Show)

data Email = Email {
    toAddresses      :: Recipients
  , emailCc          :: CarbonCopies
  , emailBcc         :: BlindCarbonCopies

  , fromAddress      :: FromAddress
  , senderName       :: SenderName
  , emailBody        :: EmailBody
  , emailSubject     :: EmailSubject
  } deriving (Eq, Show)
