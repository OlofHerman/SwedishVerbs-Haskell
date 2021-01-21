module Main where

import Api (app)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite
  ( runMigration,
    runSqlPool,
    withSqlitePool,
  )
import Models (migrateAll)
import Network.Wai.Handler.Warp (run)
import RIO (IO, MonadIO (liftIO), flip, ($))

main :: IO ()
main = runStderrLoggingT $
  withSqlitePool "verbdb.sqlite" 3 $
    \pool -> liftIO $ run 8080 $ app pool
