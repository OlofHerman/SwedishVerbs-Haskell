{-# LANGUAGE TypeOperators #-}

module Lib (app) where

import Data.Aeson (ToJSON)
import RIO
  ( Eq ((==)),
    Generic,
    Maybe (..),
    Monad (return),
    Proxy (..),
    Text,
  )
import RIO.List (find)
import Servant
  ( Application,
    Get,
    Handler,
    JSON,
    QueryParam,
    Server,
    err400,
    err404,
    serve,
    throwError,
    type (:<|>) (..),
    type (:>),
  )

type VerbApi =
  "verbs" :> Get '[JSON] [SweVerb]
    :<|> "verb" :> QueryParam "infinitive" Text :> Get '[JSON] SweVerb

data SweVerb = SweVerb
  { infinitive :: Text,
    present :: Text,
    past :: Text,
    supine :: Text,
    oldInfinitive :: Text,
    oldPresent :: Text,
    oldPast :: Text,
    oldPastPlural :: Text,
    oldPresentParticle :: Text,
    verbClass :: Text
  }
  deriving (Generic)

instance ToJSON SweVerb

verbs :: [SweVerb]
verbs =
  [ SweVerb "be" "ber" "bad" "bett" "biþia" "biþer" "baþ" "bāþo" "biþin" "Vj",
    SweVerb "binda" "binder" "band" "bundit" "binda" "binder" "bant" "bundo" "bundin" "III",
    SweVerb "bita" "biter" "bet" "bitit" "bīta" "bīter" "bēt" "bitu" "bitin" "I",
    SweVerb "bjuda" "bjuder" "bjöd" "bjudit" "biūþa" "biūþer" "bø̄þ" "buþu" "buþin" "II",
    SweVerb "bära" "bär" "bar" "burit" "bæra" "bær" "bar" "bāro" "burin" "IV",
    SweVerb "driva" "driver" "drev" "drivit" "drīva" "drīver" "drēf" "drivu" "drivin" "I",
    SweVerb "dra" "drar" "drog" "dragit" "dragha" "dragher" "drōgh" "drōgho" "draghin" "VI"
  ]

verbApi :: Proxy VerbApi
verbApi = Proxy

server :: Server VerbApi
server = getAllVerbs :<|> getVerbByInf

getAllVerbs :: Servant.Handler [SweVerb]
getAllVerbs = return verbs

getVerbByInf :: Maybe Text -> Servant.Handler SweVerb
getVerbByInf maybeInfinitive = case maybeInfinitive of
  Nothing -> throwError err400
  Just maybeVerb -> case lookupVerb of
    Nothing -> throwError err404
    Just found -> return found
    where
      lookupVerb = findVerb maybeVerb verbs

findVerb :: Text -> [SweVerb] -> Maybe SweVerb
findVerb inf verbxs = find (\x -> infinitive x == inf) verbs

app :: Application
app = serve verbApi server
