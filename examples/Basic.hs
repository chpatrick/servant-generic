{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import           Data.Monoid
import           Servant
import           Servant.Generic
import qualified Data.Text as T
import           Data.Text (Text)
import           Network.Wai.Handler.Warp (run)
import           System.Timeout
import           Network.URI

data Site route = Site
  { about :: route :-
      "about" :> Get '[PlainText] Text
  , aboot :: route :-
      "aboot" :> Get '[PlainText] Text
  , faq :: route :-
      "faq" :> Get '[PlainText] Text
  , subSite :: route :-
      "subsite" :> ToServant (SubSite AsApi)
  , home :: route :-
      Get '[PlainText] Text
  } deriving Generic

data SubSite route = SubSite
  { echo :: route :-
      "echo" :> Capture "x" Text :> Get '[PlainText] Text
  , timesTwo :: route :-
      "timestwo" :> Capture "x" Int :> Get '[PlainText] Text
  } deriving Generic

type Api = ToServant (Site AsApi)

subSiteServer :: SubSite AsServer
subSiteServer = SubSite
  { echo = return
  , timesTwo = \x -> return $ T.pack $ show (x * 2)
  }

siteServer :: Site AsServer
siteServer = Site
  { about = return "about"
  , aboot = return ("did you mean " <> showLink (fieldLink about) <> "?")
  , faq = return "faq"
  , subSite = toServant subSiteServer
  , home = return "So long and thanks for all the :<|>"
  }

main :: IO ()
main = do
  let runServer = run 31337 $ serve (Proxy :: Proxy Api) (toServant siteServer)

  -- just to use this example as a test, run the server for one second
  Nothing <- timeout 1000000 runServer
  return ()

-- Servant changed the instance of MkLink in 0.10.
#if MIN_VERSION_servant(0,10,0)
showLink :: Link -> Text
showLink = toUrlPiece
#else
showLink :: URI -> Text
showLink uri = T.pack (uriToString id uri "")
#endif