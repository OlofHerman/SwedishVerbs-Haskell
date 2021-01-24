{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser)
import Database.Persist.Sqlite (Entity (entityKey, entityVal))
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import RIO (Applicative ((<*>)), Generic, String, ($), (<$>))

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    SweVerb
    infinitive String
    present String
    past String
    supine String
    oldInfinitive String
    oldPresent String
    oldPast String
    oldPastPlural String
    oldPastParticiple String
    verbClass String
    deriving Generic
|]

instance FromJSON SweVerb where
  parseJSON :: Value -> Parser SweVerb
  parseJSON = withObject "" $ \v ->
    SweVerb <$> v .: "infinitive"
      <*> v .: "present"
      <*> v .: "past"
      <*> v .: "supine"
      <*> v .: "oldInfinitive"
      <*> v .: "oldPresent"
      <*> v .: "oldPast"
      <*> v .: "oldPastPlural"
      <*> v .: "oldPastParticiple"
      <*> v .: "verbClass"

instance ToJSON SweVerb where
  toJSON :: SweVerb -> Value
  toJSON
    ( SweVerb
        infinitive
        present
        past
        supine
        oldInfinitive
        oldPresent
        oldPast
        oldPastPlural
        oldPastParticiple
        verbClass
      ) =
      object
        [ "infinitive" .= infinitive,
          "present" .= present,
          "past" .= past,
          "supine" .= supine,
          "oldInfinitive" .= oldInfinitive,
          "oldPresent" .= oldPresent,
          "oldPast" .= oldPast,
          "oldPastPlural" .= oldPastPlural,
          "oldPastParticiple" .= oldPastParticiple,
          "verbClass" .= verbClass
        ]

instance ToJSON (Entity SweVerb) where
  toJSON :: Entity SweVerb -> Value
  toJSON entityVerb = object ["id" .= entityKey entityVerb, "verb" .= entityVal entityVerb]