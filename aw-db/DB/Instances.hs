{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Instances
    (
    ) where

import           Data.Decimal (DecimalRaw, realFracToDecimal)
import           Data.Semigroup ((<>))
import qualified Data.Text as Text
import           Database.Persist (PersistField,
                                   PersistValue (PersistDouble, PersistInt64),
                                   fromPersistValue, toPersistValue)
import           Database.Persist.Sql (PersistFieldSql, SqlType (SqlReal),
                                       sqlType)

instance Integral i => PersistField (DecimalRaw i) where
    fromPersistValue = \case
        PersistInt64 i  -> pure $ fromIntegral i
        PersistDouble d -> pure $ realFracToDecimal 10 d
        v               ->
            Left $ "cannot convert " <> Text.pack (show v) <> " to DecimalRaw"
    toPersistValue = undefined

instance Integral i => PersistFieldSql (DecimalRaw i) where
    sqlType _ = SqlReal
