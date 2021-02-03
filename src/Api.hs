{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Control.Lens ((&), (.~), (?~))
import Data.Swagger
  ( HasInfo (info),
    HasProperties (properties),
    HasRequired (required),
    HasTitle (title),
    HasType (type_),
    HasVersion (version),
    NamedSchema (NamedSchema),
    Swagger,
    SwaggerType (SwaggerObject),
    ToSchema (..),
    declareSchemaRef,
  )
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
import RIO (IO, Int64, Maybe (Just, Nothing), Monad (return), MonadIO (..), Monoid (mempty), Proxy (..), ReaderT, Show (show), String, map, ($), (++), (.))
import RIO.Char (toLower)
import RIO.Prelude ((&))
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
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI
  ( SwaggerSchemaUI,
    swaggerSchemaUIServer,
  )

type VerbApi =
  "all" :> Get '[JSON] [Entity SweVerb]
    :<|> "verbs" :> QueryParam "infinitive" String :> Get '[JSON] [Entity SweVerb]
    :<|> "verb" :> Capture "id" Int64 :> Get '[JSON] (Entity SweVerb)
    :<|> "verb" :> Capture "id" Int64 :> Delete '[JSON] String
    :<|> "verb" :> ReqBody '[JSON] SweVerb :> Post '[JSON] String
    :<|> "verb" :> Capture "id" Int64 :> ReqBody '[JSON] SweVerb :> Put '[JSON] String

type Api = VerbApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

instance ToSchema (Entity SweVerb) where
  declareNamedSchema _ = do
    int64Schema <- declareSchemaRef (Proxy :: Proxy Int64)
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $
      NamedSchema (Just "Entity SweVerb") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("id", int64Schema),
                 ("infinitive", stringSchema),
                 ("present", stringSchema),
                 ("past", stringSchema),
                 ("supine", stringSchema),
                 ("oldInfinitive", stringSchema),
                 ("oldPresent", stringSchema),
                 ("oldPast", stringSchema),
                 ("oldPastPlural", stringSchema),
                 ("oldPastParticiple", stringSchema),
                 ("verbClass", stringSchema)
               ]
          & required .~ ["id"]

instance ToSchema SweVerb where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $
      NamedSchema (Just "SweVerb") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("infinitive", stringSchema),
                 ("present", stringSchema),
                 ("past", stringSchema),
                 ("supine", stringSchema),
                 ("oldInfinitive", stringSchema),
                 ("oldPresent", stringSchema),
                 ("oldPast", stringSchema),
                 ("oldPastPlural", stringSchema),
                 ("oldPastParticiple", stringSchema),
                 ("verbClass", stringSchema)
               ]
          & required
            .~ [ "infinitive",
                 "present",
                 "past",
                 "supine",
                 "oldInfinitive",
                 "oldPresent",
                 "oldPast",
                 "oldPastPlural",
                 "oldPastParticiple",
                 "verbClass"
               ]

server :: ConnectionPool -> Server Api
server pool =
  ( getAllVerbs
      :<|> getVerbsByInf
      :<|> getVerbById
      :<|> deleteVerbById
      :<|> postVerb
      :<|> updateVerbById
  )
    :<|> swaggerSchemaUIServer swaggerDoc
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

verbApi :: Proxy Api
verbApi = Proxy

runDb :: MonadIO m => ConnectionPool -> ReaderT SqlBackend IO a -> m a
runDb pool query = liftIO $ runSqlPool query pool

app :: ConnectionPool -> Application
app pool = serve verbApi $ server pool

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy VerbApi)
    & info . title .~ "Swedish Verbs API"
    & info . version .~ "0.3.0"