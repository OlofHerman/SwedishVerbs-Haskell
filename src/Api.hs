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
    (==.),
  )
import Models (EntityField (SweVerbInfinitive), SweVerb)
import RIO
  ( IO,
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
  "verbs" :> Get '[JSON] [SweVerb]
    :<|> "verb" :> QueryParam "infinitive" String :> Get '[JSON] SweVerb
    :<|> "verb" :> QueryParam "infinitive" String :> Delete '[JSON] String
    :<|> "verb" :> ReqBody '[JSON] SweVerb :> Post '[JSON] String

server :: ConnectionPool -> Server VerbApi
server pool = getAllVerbs :<|> getVerbByInf :<|> deleteVerbByInf :<|> postVerb
  where
    getAllVerbs :: Handler [SweVerb]
    getAllVerbs = do
      entities <- runDb pool $ selectList [] []
      return $ map entityVal entities

    getVerbByInf :: Maybe String -> Handler SweVerb
    getVerbByInf maybeInf = case maybeInf of
      Nothing -> throwError $ err400 {errBody = "No infinitive supplied."}
      Just maybeVerb -> do
        lookupVerb <- runDb pool $ selectFirst [SweVerbInfinitive ==. map toLower maybeVerb] []
        case lookupVerb of
          Nothing -> throwError $ err404 {errBody = "Verb not found."}
          Just found -> return $ entityVal found

    postVerb :: SweVerb -> Handler String
    postVerb pverb = do
      newVerbKey <- runDb pool $ insert pverb
      return $ "Verb inserted with id: " ++ show (fromSqlKey newVerbKey)

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

verbApi :: Proxy VerbApi
verbApi = Proxy

runDb :: MonadIO m => ConnectionPool -> ReaderT SqlBackend IO a -> m a
runDb pool query = liftIO $ runSqlPool query pool

app :: ConnectionPool -> Application
app pool = serve verbApi $ server pool
