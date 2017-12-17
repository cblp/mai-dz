{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB
    (
    -- * DB schema
    Product (..),
    ProductId,
    pProduct,
    WorkOrder (..),
    WorkOrderId,
    -- * DB tools
    DB,
    SqlTable,
    -- * Persist re-exports
    DBName (..),
    Entity (..),
    EntityDef (..),
    FieldDef (..),
    PersistEntity (..),
    PersistField (..),
    PersistValue (..),
    SqlBackend,
    fromPersistValueText,
    runDB,
    selectList,
    ) where

import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Decimal (Decimal)
import           Data.Proxy (Proxy (Proxy))
import           Data.Text (Text)
import           Database.Persist (DBName (..), Entity (..), EntityDef (..),
                                   FieldDef (..), PersistEntity (..),
                                   PersistField (..), PersistValue (..),
                                   fromPersistValueText, selectList)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Sqlite (runSqlite)
import           Database.Persist.TH (mkPersist, persistUpperCase, share,
                                      sqlSettings)

import           DB.Instances ()

type SqlTable record =
    ( PersistEntity record
    , PersistEntityBackend record ~ SqlBackend
    , PersistField record
    )

type Name = Text
type DateTime = Text
type Money = Decimal
type UniqueIdentifier = Text

share
    [mkPersist sqlSettings]
    [persistUpperCase|
        Product
            Id                                  sql=ProductID
            name                  Name
            productNumber         Text
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
            rowguid               UniqueIdentifier
            modifiedDate          DateTime

            deriving Show

        WorkOrder
            Id                          sql=WorkOrderID
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

pProduct :: Proxy Product
pProduct = Proxy

type DB = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

runDB :: DB a -> IO a
runDB = runSqlite "db.sqlite"
