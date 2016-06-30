module Lib
    ( startApp
    ) where

-- Prelude imports
import Data.Int(Int)
import Data.Eq(Eq)
import Data.Monoid((<>))
import Text.Show(show)
import System.IO(IO)
import System.IO(putStrLn)
import Control.Monad
import Data.Function
import Data.List
import Control.Monad.IO.Class
import Data.Functor(fmap)
import Data.Maybe(fromJust)
-- import Control.Monad.Trans(liftIO)
import Data.String(String)
import qualified Data.String as String
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import Data.ByteString.Lazy(fromStrict)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- import Network.HTTP hiding(Response)
import Api

-- $(deriveJSON defaultOptions ''User)

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import Network.HTTP hiding (Response,
                            getResponseBody)
import Network.HTTP.Simple hiding(Proxy)
import Network.URI


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
--server = (return users) :<|> echo :<|> handleCommand
server = (return users) :<|> handleCommand

-- users
users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

-- ECHO
printHeaders hs =
  unlines $ fmap (\(h,v)-> (show h) ++ "->"++ (show v) ++ "\n") hs

echo request respond = do
  body <- requestBody request >>= return.fromStrict
  let method = requestMethod request
  let rawHeaders = requestHeaders request
  let bodyStr = (LC8.concat ["Method:\n\n", (LC8.pack . show) method, "\n",
                              "Headers:\n\n", (LC8.pack . printHeaders) rawHeaders, "\n",
                              "Body:\n\n", body, "\n",
                              "-------------------------------"
                            ])

  let response = responseLBS status200 [("Content-Type", "text/plain")] bodyStr
  LC8.putStrLn bodyStr

  respond response


-- handle Command
handleCommand :: Message -> Handler String
handleCommand m =
  do
    (liftIO . putStrLn . show) m
    let response = Response Ephemeral (show m)
    req <- parseRequest $ Text.unpack $ ("POST " <> (response_url m))
    let req' = setRequestBodyJSON response req
    (liftIO . putStrLn . show) m
    (liftIO . putStrLn . show) req'
    resp <- httpLBS $ req'
    --httpResponse <- httpJSON request
    return $ LC8.unpack $ getResponseBody resp
    -- resp <- liftIO $ simpleHTTP(postRequestWithBody (Text.unpack $ response_url m)
    --                              "application/json"
    --                              (LC8.unpack $ encode response))
