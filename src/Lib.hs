{-# LANGUAGE TypeOperators #-}

module Lib (app) where

import Data.Aeson (ToJSON)
import RIO
  ( Eq ((==)),
    Generic,
    Integer,
    Maybe (..),
    Monad (return),
    Proxy (..),
    Text,
  )
import RIO.List (find)
import Servant
  ( Application,
    Capture,
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
    :<|> "id" :> Capture "id" Integer :> Get '[JSON] SweVerb

data SweVerb = SweVerb
  { id :: Integer,
    infinitive :: Text,
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
  [ SweVerb 1 "be" "ber" "bad" "bett" "biþia" "biþer" "baþ" "bāþo" "biþin" "Vj",
    SweVerb 2 "binda" "binder" "band" "bundit" "binda" "binder" "bant" "bundo" "bundin" "III",
    SweVerb 3 "bita" "biter" "bet" "bitit" "bīta" "bīter" "bēt" "bitu" "bitin" "I",
    SweVerb 4 "bjuda" "bjuder" "bjöd" "bjudit" "biūþa" "biūþer" "bø̄þ" "buþu" "buþin" "II",
    SweVerb 5 "bära" "bär" "bar" "burit" "bæra" "bær" "bar" "bāro" "burin" "IV",
    SweVerb 6 "driva" "driver" "drev" "drivit" "drīva" "drīver" "drēf" "drivu" "drivin" "I",
    SweVerb 7 "dra" "drar" "drog" "dragit" "dragha" "dragher" "drōgh" "drōgho" "draghin" "VI"
  ]

verbApi :: Proxy VerbApi
verbApi = Proxy

server :: Server VerbApi
server = getAllVerbs :<|> getVerbByInf :<|> getVerbById

getAllVerbs :: Servant.Handler [SweVerb]
getAllVerbs = return verbs

getVerbByInf :: Maybe Text -> Servant.Handler SweVerb
getVerbByInf maybeInfinitive = case maybeInfinitive of
  Nothing -> throwError err400
  Just maybeVerb -> case lookupVerb of
    Nothing -> throwError err404
    Just found -> return found
    where
      lookupVerb = findVerbByInf maybeVerb

findVerbByInf :: Text -> Maybe SweVerb
findVerbByInf inf = find (\x -> infinitive x == inf) verbs

getVerbById :: Integer -> Servant.Handler SweVerb
getVerbById verbId = case lookupVerb of
  Nothing -> throwError err404
  Just found -> return found
  where
    lookupVerb = find (\x -> id x == verbId) verbs

app :: Application
app = serve verbApi server
