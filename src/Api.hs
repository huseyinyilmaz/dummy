{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

-- Prelude imports
import Data.Int(Int)
import Data.Eq(Eq)
import Text.Show(Show)
import System.IO(IO)
import Control.Monad(return)
import Data.String(String)

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

type UsersAPI = "users" :> Get '[JSON] [User]
type PrintAPI = "print" :> ReqBody '[PlainText] String :> Get '[PlainText] String
type API = UsersAPI :<|> PrintAPI
