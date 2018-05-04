{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Tabs
    ( addQueryTab
    , displayBom
    , displayWorkOrder
    ) where

import           Prelude hiding (product)

import           Control.Monad
import           Data.Char
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.List (isInfixOf)
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Traversable
import           Database.Persist
import           Foreign.Hoppy.Runtime

import           QAbstractButton
import           QAbstractItemView
import           QBoxLayout
import           QGridLayout
import           QLineEdit
import           QMessageBox
import           QPushButton
import           Qt.Signal
import           QTabWidget
import           QtCore
import           QTreeView
import           QTreeWidget
import           QTreeWidgetItem
import           QVariant
import           QVBoxLayout
import           QWidget (QWidget, setLayout)
import qualified QWidget

import           DB

addQueryTab
    :: SqlTable record
    => QTabWidget
    -> String -- ^ tab name
    -> Maybe String -- ^ search hint
    -> [Filter record]
    -> IO (QGridLayout, QTreeWidget) -- ^ tab's layout and view of queried items
addQueryTab tabs name mSearchHint query = do
    (tab, toolBarGrid, view) <- makeQueryTab mSearchHint query
    void $ addTab tabs tab name
    setCurrentWidget tabs tab
    pure (toolBarGrid, view)

addBomTab :: QTabWidget -> (Name, ProductId) -> IO ()
addBomTab tabs (prodName, prodId) = do
    tab <- makeBomView (prodName, prodId)
    void
        $  addTab tabs tab
        $  "Компоненты (BillOfMaterials) для "
        ++ Text.unpack prodName
    setCurrentWidget tabs tab

makeQueryTab
    :: SqlTable record
    => Maybe String -- ^ search hint
    -> [Filter record]
    -> IO (QWidget, QGridLayout, QTreeWidget) -- ^ tab, its layout, and view of queried items
makeQueryTab mSearchHint query = do
    tab    <- QWidget.new
    tabBox <- QVBoxLayout.new
    setLayout tab tabBox

    toolBar <- QWidget.new
    QBoxLayout.addWidget tabBox toolBar
    toolBarGrid <- QGridLayout.new
    setVerticalSpacing toolBarGrid 0
    setLayout          toolBar     toolBarGrid

    search <- QLineEdit.new
    setPlaceholderText    search "Фильтр..."
    setClearButtonEnabled search True
    QGridLayout.addWidget toolBarGrid search 0 0

    case mSearchHint of
        Nothing   -> pure ()
        Just hint -> do
            searchHintButton <- QPushButton.newWithText $ "Например, " ++ hint
            setFlat               searchHintButton True
            QWidget.setStyleSheet searchHintButton "text-align: left;"
            connect_              searchHintButton QAbstractButton.clickedSignal
                $ \_ -> QLineEdit.setText search hint
            QGridLayout.addWidget toolBarGrid searchHintButton 1 0

    view <- makeQueryView query
    QBoxLayout.addWidget tabBox view

    connect_             search textChangedSignal $ updateViewWithSearch view

    pure (tab, toolBarGrid, view)

makeQueryView
    :: forall record . SqlTable record => [Filter record] -> IO QTreeWidget
makeQueryView query = do
    view <- QTreeWidget.new
    setAlternatingRowColors view True
    setHeaderLabels view $ map (Text.unpack . unDBName . fieldDB) fields
    loadQueryResult view query
    pure view
    where fields = entityFields $ entityDef (Proxy :: Proxy record)

makeBomView :: (Name, ProductId) -> IO QTreeWidget
makeBomView product = do
    view <- QTreeWidget.new
    setAlternatingRowColors view True
    setHeaderLabels         view ["Name"]
    root <- invisibleRootItem view
    connect_ view itemExpandedSignal loadBomChildren
    item <- addBomItem root product
    setCurrentItem view item
    pure view

loadQueryResult
    :: forall record
     . SqlTable record
    => QTreeWidget
    -> [Filter record]
    -> IO ()
loadQueryResult view query = do
    items :: [Entity record] <- runDB $ selectList query []
    for_ items $ \(Entity itemId record) -> do
        row <- case toPersistValue record of
            PersistMap row -> pure $ map snd row
            value          -> error $ show value
        labels <- for row $ \field -> case field of
            PersistNull -> pure ""
            _           -> case fromPersistValueText field of
                Left  e -> error $ Text.unpack e
                Right r -> pure $ Text.unpack r
        item <- QTreeWidgetItem.newWithParentTreeAndStrings view labels
        setRecordId item (coerce itemId :: Int)
    for_ [0 .. length fields - 1] $ resizeColumnToContents view

    unless (null items) $ setCurrentItem view =<< topLevelItem view 0

    -- In order to avoid performance issues, it is recommended that sorting is
    -- enabled after inserting the items into the tree.
    setSortingEnabled view True
    sortItems view 0 AscendingOrder
    where fields = entityFields $ entityDef (Proxy :: Proxy record)

updateViewWithSearch :: QTreeWidget -> String -> IO ()
updateViewWithSearch view searchTerms = do
    items <- topLevelItemCount view
    for_ [0 .. items - 1] $ \i -> do
        item    <- topLevelItem view i
        matched <- case searchTerms of
            "" -> pure True
            _  -> do
                columns <- QTreeWidgetItem.columnCount item
                matched <- for [0 .. columns - 1] $
                    fmap match . QTreeWidgetItem.text item
                pure $ or matched
        setHidden item $ not matched
    where match txt = map toLower searchTerms `isInfixOf` map toLower txt

displayWorkOrder :: QTabWidget -> QTreeWidget -> IO ()
displayWorkOrder tabs view = do
    item <- currentItem view
    if item /= nullptr
        then do
            prodId <- getRecordId item
            name   <- QTreeWidgetItem.text item 0
            void $ addQueryTab tabs
                               ("Заказы (WorkOrder) для " ++ name)
                               Nothing
                               [WorkOrderProductID ==. ProductKey prodId]
        else void $ QMessageBox.critical
            view
            "Не выбран продукт"
            "Выберите продукт для отображения заказов"

displayBom :: QTabWidget -> QTreeWidget -> IO ()
displayBom tabs view = do
    item <- currentItem view
    if item /= nullptr
        then do
            prodId   <- getRecordId item
            prodName <- QTreeWidgetItem.text item 0
            addBomTab tabs (Text.pack prodName, ProductKey prodId)
        else void $ QMessageBox.critical
            view
            "Не выбран продукт"
            "Выберите продукт для отображения компонентов"

addBomItem :: QTreeWidgetItem -> (Name, ProductId) -> IO QTreeWidgetItem
addBomItem parentItem (prodName, ProductKey prodId) = do
    item <- QTreeWidgetItem.newWithParentItemAndStrings
        parentItem
        [Text.unpack prodName]
    setRecordId             item prodId
    setChildIndicatorPolicy item ShowIndicator
    pure item

loadBomChildren :: QTreeWidgetItem -> IO ()
loadBomChildren parentItem = do
    prodId <- getRecordId parentItem
    boms   <- runDB $ selectBillOfMaterialsWithProductNameByProductAssemblyID
        (ProductKey prodId)
    for_ boms $ addBomItem parentItem
    setChildIndicatorPolicy parentItem DontShowIndicatorWhenChildless

recordIdRole :: Int
recordIdRole = fromEnum UserRole

getRecordId :: QTreeWidgetItem -> IO Int
getRecordId item = getData item 0 recordIdRole >>= toInt

setRecordId :: QTreeWidgetItem -> Int -> IO ()
setRecordId item = setData item 0 recordIdRole <=< QVariant.newWithInt
