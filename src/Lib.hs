module Lib
    ( startApp
    ) where

-- Prelude imports
import Data.Int(Int)
import Data.Eq(Eq)
import Text.Show(Show)
import System.IO(IO)
--import System.IO(putStrLn)
import Control.Monad(return)
import Data.Function

import Control.Monad.IO.Class
-- import Control.Monad.Trans(liftIO)

import Data.String(String)
import Data.ByteString.Lazy.Char8(putStrLn)
import Data.ByteString.Lazy.Char8(unpack)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api

-- $(deriveJSON defaultOptions ''User)


startApp :: IO ()
startApp = do
  let settings = setPort 3334 $ setHost "*" defaultSettings
  runSettings settings $ app
  -- run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
  :<|> print

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

print body = do
  liftIO $ putStrLn body
  return $ unpack body
