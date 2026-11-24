{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResultErrorSpec where

import PgInit

import Data.Time (Day)
import Database.PostgreSQL.Simple (ResultError (..))
import Database.PostgreSQL.Simple.Types (Oid (..))
import UnliftIO (try)

spec :: Spec
spec = describe "ResultError" $ do
    it "still describes the column after the statement is released" $
        runConnAssert $ do
            rawExecute "CREATE TEMPORARY TABLE bc_dates (bc_date date NOT NULL)" []
            rawExecute "INSERT INTO bc_dates VALUES ('0001-12-31 BC')" []
            [Single (tableOid :: Int)] <-
                rawSql "SELECT 'bc_dates'::regclass::oid::int" []
            result <- try (rawSql "SELECT bc_date FROM bc_dates" [])
            case result of
                Right (rows :: [Single Day]) ->
                    expectationFailure ("expected a ResultError, got " <> show rows)
                Left (err :: ResultError) -> do
                    errSQLField err `shouldBe` "bc_date"
                    fmap (\(Oid oid) -> fromIntegral oid) (errSQLTableOid err)
                        `shouldBe` Just tableOid
