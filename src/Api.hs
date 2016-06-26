{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

-- Prelude imports
import Data.Int(Int)
import Data.Eq(Eq)
import Text.Show(Show)
import System.IO(IO)
import Control.Monad(return)
import Data.String(String)
import Data.Function

-- Aeson
import Data.Aeson
import Data.Aeson.TH

import Data.ByteString.Lazy(ByteString)

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
type PrintAPI = "print" :> Raw
type API = UsersAPI :<|> PrintAPI
