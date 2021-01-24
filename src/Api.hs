{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Database.Persist.Sqlite
  ( ConnectionPool,
    Entity (entityVal),
    PersistQueryRead (selectFirst),
    PersistQueryWrite (deleteWhere),
    PersistStoreWrite (insert),
    SqlBackend,
    fromSqlKey,
    runSqlPool,
    selectList,
    toSqlKey,
    (==.),
  )
import Models (EntityField (SweVerbId, SweVerbInfinitive), SweVerb)
import RIO
  ( IO,
    Int,
    Int64,
    Maybe (Just, Nothing),
    Monad (return),
    MonadIO (..),
    Proxy (..),
    ReaderT,
    Show (show),
    String,
    map,
    ($),
    (++),
  )
import RIO.Char (toLower)
import Servant
  ( Application,
    Capture,
    Delete,
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    QueryParam,
    ReqBody,
    Server,
    ServerError (errBody),
    err400,
    err404,
    serve,
    throwError,
    type (:<|>) (..),
    type (:>),
  )

type VerbApi =
  "verbs" :> Get '[JSON] [Entity SweVerb]
    :<|> "verb" :> Capture "id" Int64 :> Get '[JSON] (Entity SweVerb)
    :<|> "verb" :> QueryParam "infinitive" String :> Get '[JSON] (Entity SweVerb)
    :<|> "verb" :> QueryParam "infinitive" String :> Delete '[JSON] String
    :<|> "verb" :> ReqBody '[JSON] SweVerb :> Post '[JSON] String

server :: ConnectionPool -> Server VerbApi
server pool = getAllVerbs :<|> getVerbById :<|> getVerbByInf :<|> deleteVerbByInf :<|> postVerb
  where
    getAllVerbs :: Handler [Entity SweVerb]
    getAllVerbs = runDb pool $ selectList [] []

    getVerbById :: Int64 -> Handler (Entity SweVerb)
    getVerbById dbId = do
      lookupVerb <- runDb pool $ selectFirst [SweVerbId ==. toSqlKey dbId] []
      case lookupVerb of
        Nothing -> throwError $ err404 {errBody = "Verb not found."}
        Just found -> return found

    getVerbByInf :: Maybe String -> Handler (Entity SweVerb)
    getVerbByInf maybeInf = case maybeInf of
      Nothing -> throwError $ err400 {errBody = "No infinitive supplied."}
      Just maybeVerb -> do
        lookupVerb <- runDb pool $ selectFirst [SweVerbInfinitive ==. map toLower maybeVerb] []
        case lookupVerb of
          Nothing -> throwError $ err404 {errBody = "Verb not found."}
          Just found -> return found

    deleteVerbByInf :: Maybe String -> Handler String
    deleteVerbByInf maybeInf = case maybeInf of
      Nothing -> throwError $ err400 {errBody = "No infinitive supplied."}
      Just maybeVerb -> do
        lookupVerb <- runDb pool $ selectFirst [SweVerbInfinitive ==. maybeVerb] []
        case lookupVerb of
          Nothing -> throwError err404 {errBody = "Verb not found."}
          Just found -> do
            _ <- runDb pool $ deleteWhere [SweVerbInfinitive ==. maybeVerb]
            return "Verb deleted."

    postVerb :: SweVerb -> Handler String
    postVerb pverb = do
      newVerbKey <- runDb pool $ insert pverb
      return $ "Verb inserted with id: " ++ show (fromSqlKey newVerbKey)

-- updateVerbById :: Int64 -> SweVerb -> Handler String
-- updateVerbById dbId updateVerb =

verbApi :: Proxy VerbApi
verbApi = Proxy

runDb :: MonadIO m => ConnectionPool -> ReaderT SqlBackend IO a -> m a
runDb pool query = liftIO $ runSqlPool query pool

app :: ConnectionPool -> Application
app pool = serve verbApi $ server pool
