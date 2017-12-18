{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           Prelude hiding (product)

import           Control.Monad (void)
import           Data.Char (toLower)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.List (isInfixOf)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Database.Persist (DBName (..), Entity (..), EntityDef (..),
                                   FieldDef (..), Filter, PersistEntity (..),
                                   PersistField (..), PersistValue (..),
                                   fromPersistValueText, selectList, (==.))
import           Foreign.Hoppy.Runtime (delete, nullptr, withScopedPtr)
import           System.Environment (getArgs)

import           QAbstractButton
import           QAbstractItemView
import           QApplication
import           QBoxLayout
import           QCoreApplication
import           QHBoxLayout
import           QLineEdit
import           QMainWindow
import           QMessageBox
import           QPushButton
import           QShowEvent
import           Qt.Event
import           Qt.Signal
import           QTabWidget
import           QtCore
import           QTreeView
import           QTreeWidget
import           QTreeWidgetItem
import           QVariant
import           QVBoxLayout
import           QWidget (QWidget, cast, setLayout, showMaximized)
import qualified QWidget

import           DB

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeMainWindow
    showMaximized mainWindow
    exec
  where
    withApp = withScopedPtr $ do
        args <- getArgs
        QApplication.new args

makeMainWindow :: IO QWidget
makeMainWindow = do
    mainWindow <- QMainWindow.new
    setUnifiedTitleAndToolBarOnMac mainWindow True

    tabs <- QTabWidget.new
    setTabsClosable tabs True
    connect_        tabs tabCloseRequestedSignal $ closeTab tabs
    do
        (toolBarL, view) <- addQueryTab @Product tabs "Продукция (Product)" []
        do
            displayWorkOrderButton <- QPushButton.newWithText
                "Заказы (WorkOrder)"
            connect_ displayWorkOrderButton QAbstractButton.clickedSignal
                $ \_ -> displayWorkOrder tabs view
            addWidget toolBarL displayWorkOrderButton
        do
            displayBillOfMaterialsButton <- QPushButton.newWithText
                "Компоненты (BillOfMaterials)"
            connect_ displayBillOfMaterialsButton QAbstractButton.clickedSignal
                $ \_ -> displayBillOfMaterials tabs view
            addWidget toolBarL displayBillOfMaterialsButton
    setCentralWidget mainWindow tabs

    pure (QWidget.cast mainWindow)

addQueryTab
    :: SqlTable record
    => QTabWidget
    -> String -- ^ tab name
    -> [Filter record]
    -> IO (QBoxLayout, QTreeWidget) -- ^ tab's layout and view of queried items
addQueryTab tabs name queryFilters = do
    (tab, toolBarL, view) <- makeQueryTab queryFilters
    void $ addTab tabs tab name
    setCurrentWidget tabs tab
    pure (toolBarL, view)

addBomTab :: QTabWidget -> (String, ProductId) -> IO ()
addBomTab tabs (prodName, prodId) = do
    tab <- makeBillOfMaterialsView (prodName, prodId)
    void $ addTab tabs tab $ "Компоненты (BillOfMaterials) для " ++ prodName
    setCurrentWidget tabs tab

makeQueryTab
    :: SqlTable record
    => [Filter record]
    -> IO (QWidget, QBoxLayout, QTreeWidget) -- ^ tab, its layout, and view of queried items
makeQueryTab queryFilters = do
    tab  <- QWidget.new
    tabL <- QVBoxLayout.new
    setLayout tab tabL

    toolBar <- QWidget.new
    addWidget tabL toolBar
    toolBarL <- QHBoxLayout.new
    setLayout toolBar toolBarL

    search <- QLineEdit.new
    setPlaceholderText    search   "Фильтр..."
    setClearButtonEnabled search   True
    addWidget             toolBarL search

    view <- makeQueryView queryFilters
    addWidget tabL   view

    connect_  search textChangedSignal $ updateViewWithSearch view

    pure (tab, QBoxLayout.cast toolBarL, view)

makeQueryView
    :: forall record . SqlTable record => [Filter record] -> IO QTreeWidget
makeQueryView queryFilters = do
    view <- QTreeWidget.new
    setAlternatingRowColors view True
    setHeaderLabels view $ map (Text.unpack . unDBName . fieldDB) fields
    void $ onEvent view $ \(_ :: QShowEvent) -> do
        loadQueryResult view queryFilters
        pure True
    pure view
    where fields = entityFields $ entityDef (Proxy :: Proxy record)

makeBillOfMaterialsView :: (String, ProductId) -> IO QTreeWidget
makeBillOfMaterialsView product = do
    view <- QTreeWidget.new
    setAlternatingRowColors view True
    setHeaderLabels         view ["Name"]
    root <- invisibleRootItem view
    addBomItem root product
    pure view

loadQueryResult
    :: forall record
     . SqlTable record
    => QTreeWidget
    -> [Filter record]
    -> IO ()
loadQueryResult view queryFilters = do
    items :: [Entity record] <- runDB (selectList queryFilters [])
    for_ items $ \(Entity itemId record) -> do
        row <- case toPersistValue record of
            -- PersistList row -> pure row -- TODO remove?
            PersistMap row -> pure $ map snd row
            value          -> error $ show value
        labels <- for row $ \field -> case field of
            PersistNull -> pure ""
            _           -> case fromPersistValueText field of
                Left  e -> error $ Text.unpack e
                Right r -> pure $ Text.unpack r
        item <- QTreeWidgetItem.newWithParentTreeAndStrings view labels
        setData item 0 (fromEnum UserRole)
            =<< QVariant.newWithInt (coerce itemId :: Int)
    for_ [0 .. length fields - 1] $ resizeColumnToContents view

    -- In order to avoid performance issues, it is recommended that sorting is
    -- enabled after inserting the items into the tree.
    setSortingEnabled view True
    where fields = entityFields $ entityDef (Proxy :: Proxy record)

updateViewWithSearch :: QTreeWidget -> String -> IO ()
updateViewWithSearch view searchTerms = do
    items <- topLevelItemCount view
    for_ [0 .. items - 1] $ \i -> do
        item    <- topLevelItem view i
        matched <- case searchTerms of
            "" -> pure True
            _  -> do
                columns <- columnCount item
                matched <- for [0 .. columns - 1] $ \j -> do
                    cellText <- QTreeWidgetItem.text item j
                    pure $ match cellText
                pure $ or matched
        setHidden item $ not matched
    where match txt = map toLower searchTerms `isInfixOf` map toLower txt

displayWorkOrder :: QTabWidget -> QTreeWidget -> IO ()
displayWorkOrder tabs view = do
    item <- currentItem view
    if item /= nullptr
        then do
            prodId <- getData item 0 (fromEnum UserRole) >>= toInt
            name   <- QTreeWidgetItem.text item 0
            void $ addQueryTab tabs
                               ("Заказы (WorkOrder) для " ++ name)
                               [WorkOrderProductID ==. ProductKey prodId]
        else void $ QMessageBox.critical
            view
            "Не выбран продукт"
            "Выберите продукт для отображения заказов"

displayBillOfMaterials :: QTabWidget -> QTreeWidget -> IO ()
displayBillOfMaterials tabs view = do
    item <- currentItem view
    if item /= nullptr
        then do
            prodId   <- getData item 0 (fromEnum UserRole) >>= toInt
            prodName <- QTreeWidgetItem.text item 0
            addBomTab tabs (prodName, ProductKey prodId)
        else void $ QMessageBox.critical
            view
            "Не выбран продукт"
            "Выберите продукт для отображения компонентов"

closeTab :: QTabWidget -> Int -> IO ()
closeTab _    0 = pure ()
closeTab tabs i = delete =<< QTabWidget.widget tabs i

addBomItem :: QTreeWidgetItem -> (String, ProductId) -> IO ()
addBomItem parentItem (prodName, ProductKey prodId) = do
    item <- QTreeWidgetItem.newWithParentItemAndStrings parentItem [prodName]
    setData item 0 (fromEnum UserRole)
        =<< QVariant.newWithInt (coerce prodId :: Int)
    setChildIndicatorPolicy item ShowIndicator
