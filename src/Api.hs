module Api where

-- Prelude imports
import Data.Int(Int)
import Data.Eq(Eq)
import Text.Show(Show)
import System.IO(IO)
import Control.Monad(return)
import Data.String(String)
import Control.Applicative
import Data.List(lookup)
import Data.Maybe
import Data.Either
import Data.Monoid
import Data.Function
import Data.Text
-- Aeson
import Data.Aeson
import Data.Aeson.TH

-- Servant
--import Data.Text
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data Message = Message
  { token :: Text
  , team_id :: Text
  , team_domain :: Text
  , channel_id :: Text
  , channel_name :: Text
  , user_id :: Text
  , user_name :: Text
  , command :: Text
  , text :: Text
  , response_url :: Text
  } deriving (Show)

instance FromFormUrlEncoded Message where
  fromFormUrlEncoded inputs =
    Message <$> lkp "token"
            <*> lkp "team_id"
            <*> lkp "team_domain"
            <*> lkp "channel_id"
            <*> lkp "channel_name"
            <*> lkp "user_id"
            <*> lkp "user_name"
            <*> lkp "command"
            <*> lkp "text"
            <*> lkp "response_url"

    where lkp input_label = case lookup input_label inputs of
                 Nothing -> Left $ "label " <> (unpack input_label) <> " not found"
                 Just v    -> Right v


type UsersAPI = "users" :> Get '[JSON] [User]
type PrintAPI = "echo" :> ReqBody '[PlainText] String :> Get '[PlainText] String
type CommandAPI = "command" :> ReqBody '[FormUrlEncoded] Message :> Post '[PlainText] String
--type API = UsersAPI :<|> PrintAPI :<|> CommandAPI
type API = UsersAPI :<|> CommandAPI
