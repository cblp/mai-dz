{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB where

import           Data.Coerce (Coercible)
import           Data.Decimal (Decimal)
import           Data.Text (Text)
import           Database.Persist (PersistEntity (..), PersistField (..))
import           Database.Persist.Sql (SqlBackend, SqlPersistM)
import           Database.Persist.Sqlite (runSqlite)
import           Database.Persist.TH (mkPersist, persistUpperCase, sqlSettings)

import           DB.Instances ()

type SqlTable record =
    ( PersistEntity record
    , PersistEntityBackend record ~ SqlBackend
    , PersistField record
    , Coercible (Key record) Int
    )

type Name = Text
type DateTime = Text
type Money = Decimal
type UniqueIdentifier = Text

mkPersist
    sqlSettings
    [persistUpperCase|
        -- https://technet.microsoft.com/ru-ru/library/ms124719(v=sql.100).aspx
        Product
            Id                    Int           sql=ProductID   -- PRIMARY
            name                  Name                          -- UNIQUE
            productNumber         Text                          -- UNIQUE
            makeFlag              Bool
            finishedGoodsFlag     Bool
            color                 Text Maybe
            safetyStockLevel      Int
            reorderPoint          Int
            standardCost          Money
            listPrice             Money
            size                  Text Maybe
            sizeUnitMeasureCode   Text Maybe
            weightUnitMeasureCode Text Maybe
            weight                Decimal Maybe
            daysToManufacture     Int
            productLine           Text Maybe
            class                 Text Maybe
            style                 Text Maybe
            productSubcategoryID  Int Maybe
            productModelID        Int Maybe
            sellStartDate         DateTime
            sellEndDate           DateTime Maybe
            discontinuedDate      DateTime Maybe
            rowguid               UniqueIdentifier              -- UNIQUE
            modifiedDate          DateTime

            deriving Show

        -- https://technet.microsoft.com/ru-ru/library/ms124622(v=sql.100).aspx
        WorkOrder
            Id            Int           sql=WorkOrderID   -- PRIMARY
            productID     ProductId
            orderQty      Int
            stockedQty    Int
            scrappedQty   Int
            startDate     DateTime
            endDate       DateTime Maybe
            dueDate       DateTime
            scrapReasonID Int Maybe
            modifiedDate  DateTime
    |]

runDB :: SqlPersistM a -> IO a
runDB = runSqlite "db.sqlite"
