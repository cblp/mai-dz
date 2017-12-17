{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad (void)
import           Data.Char (toLower)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.List (isInfixOf)
import           Data.Proxy (Proxy)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Database.Persist (DBName (..), Entity (..), EntityDef (..),
                                   FieldDef (..), Filter, PersistEntity (..),
                                   PersistField (..), PersistValue (..),
                                   fromPersistValueText, selectList, (==.))
import           Foreign.Hoppy.Runtime (nullptr, withScopedPtr)
import           System.Environment (getArgs)

import           QAbstractButton
import           QAbstractItemView
import           QApplication
import           QBoxLayout
import           QCoreApplication
import           QLineEdit
import           QMainWindow
import           QMessageBox
import           QPushButton
import           QTabWidget
import           QtCore
import           QToolBar
import           QTreeView
import           QTreeWidget
import           QTreeWidgetItem
import           QVariant
import           QVBoxLayout
import           QWidget (QWidget, cast, setLayout, showMaximized)
import qualified QWidget
import           Signal

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
    do
        (toolBar, view) <- addQueryTab tabs "Продукция (Product)" pProduct []
        displayWorkOrderButton <- QPushButton.newWithText "Заказы (WorkOrder)" -- TODO QToolButton
        connect_ displayWorkOrderButton QAbstractButton.clickedSignal
            $ \_ -> displayWorkOrder tabs view
        void $ QToolBar.addWidget toolBar displayWorkOrderButton
    setCentralWidget mainWindow tabs

    pure (QWidget.cast mainWindow)

addQueryTab
    :: SqlTable record
    => QTabWidget
    -> String
    -> Proxy record
    -> [Filter record]
    -> IO (QToolBar, QTreeWidget)
addQueryTab tabs name pTable queryFilters = do
    (tab, toolBar, view) <- makeQueryTab pTable queryFilters
    void $ addTab tabs tab name
    setCurrentWidget tabs tab
    pure (toolBar, view)

makeQueryTab
    :: SqlTable record
    => Proxy record
    -> [Filter record]
    -> IO (QWidget, QToolBar, QTreeWidget)
makeQueryTab pTable queryFilters = do
    tab  <- QWidget.new
    tabL <- QVBoxLayout.new
    setLayout tab tabL

    toolBar <- QToolBar.new
    QBoxLayout.addWidget tabL toolBar

    search <- QLineEdit.new
    setPlaceholderText    search "Фильтр..."
    setClearButtonEnabled search True
    void $ QToolBar.addWidget toolBar search

    view <- makeQueryView pTable queryFilters
    QBoxLayout.addWidget tabL   view

    connect_             search textChangedSignal $ updateViewWithSearch view

    pure (tab, toolBar, view)

makeQueryView
    :: forall record
     . SqlTable record
    => Proxy record
    -> [Filter record]
    -> IO QTreeWidget
makeQueryView pTable queryFilters = do
    view <- QTreeWidget.new
    setAlternatingRowColors view True

    setHeaderLabels view $ map (Text.unpack . unDBName . fieldDB) fields

    items :: [Entity record] <- runDB (selectList queryFilters [])
    for_ items $ \(Entity itemId record) -> do
        row <- case toPersistValue record of
            PersistList row -> pure row
            PersistMap  row -> pure $ map snd row
            value           -> error $ show value
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

    pure view
    where fields = entityFields $ entityDef pTable

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
            productId <- getData item 0 (fromEnum UserRole) >>= toInt
            name      <- QTreeWidgetItem.text item 0
            void $ addQueryTab tabs
                               ("Заказы (WorkOrder) для " ++ name)
                               pWorkOrder
                               [WorkOrderProductID ==. ProductKey productId]
        else void $ QMessageBox.critical
            view
            "Не выбран продукт"
            "Выберите продукт для отображения заказов"
