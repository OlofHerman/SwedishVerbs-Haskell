{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Database.Persist.Sqlite
  ( ConnectionPool,
    Entity (entityVal),
    PersistQueryRead (selectFirst),
    PersistQueryWrite (deleteWhere),
    PersistStoreWrite (insert, replace),
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
    Put,
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
    :<|> "verbs" :> QueryParam "infinitive" String :> Get '[JSON] [Entity SweVerb]
    :<|> "verb" :> Capture "id" Int64 :> Get '[JSON] (Entity SweVerb)
    :<|> "verb" :> Capture "id" Int64 :> Delete '[JSON] String
    :<|> "verb" :> ReqBody '[JSON] SweVerb :> Post '[JSON] String
    :<|> "verb" :> Capture "id" Int64 :> ReqBody '[JSON] SweVerb :> Put '[JSON] String

server :: ConnectionPool -> Server VerbApi
server pool =
  getAllVerbs
    :<|> getVerbsByInf
    :<|> getVerbById
    :<|> deleteVerbById
    :<|> postVerb
    :<|> updateVerbById
  where
    getAllVerbs :: Handler [Entity SweVerb]
    getAllVerbs = runDb pool $ selectList [] []

    getVerbsByInf :: Maybe String -> Handler [Entity SweVerb]
    getVerbsByInf maybeInf = case maybeInf of
      Nothing -> throwError $ err400 {errBody = "No infinitive supplied."}
      Just maybeVerb -> runDb pool $ selectList [SweVerbInfinitive ==. map toLower maybeVerb] []

    getVerbById :: Int64 -> Handler (Entity SweVerb)
    getVerbById dbId = do
      lookupVerb <- runDb pool $ selectFirst [SweVerbId ==. toSqlKey dbId] []
      case lookupVerb of
        Nothing -> throwError $ err404 {errBody = "Verb not found."}
        Just found -> return found

    deleteVerbById :: Int64 -> Handler String
    deleteVerbById dbId = do
      lookupVerb <- runDb pool $ selectFirst [SweVerbId ==. toSqlKey dbId] []
      case lookupVerb of
        Nothing -> throwError $ err404 {errBody = "Verb not found."}
        Just found -> do
          _ <- runDb pool $ deleteWhere [SweVerbId ==. toSqlKey dbId]
          return "Verb deleted."

    postVerb :: SweVerb -> Handler String
    postVerb pverb = do
      newVerbKey <- runDb pool $ insert pverb
      return $ "Verb inserted with id: " ++ show (fromSqlKey newVerbKey)

    updateVerbById :: Int64 -> SweVerb -> Handler String
    updateVerbById dbId updateVerb = do
      lookupVerb <- runDb pool $ selectFirst [SweVerbId ==. toSqlKey dbId] []
      case lookupVerb of
        Nothing -> throwError $ err404 {errBody = "Verb not found."}
        Just found -> do
          _ <- runDb pool $ replace (toSqlKey dbId) updateVerb
          return "Verb updated."

verbApi :: Proxy VerbApi
verbApi = Proxy

runDb :: MonadIO m => ConnectionPool -> ReaderT SqlBackend IO a -> m a
runDb pool query = liftIO $ runSqlPool query pool

app :: ConnectionPool -> Application
app pool = serve verbApi $ server pool
