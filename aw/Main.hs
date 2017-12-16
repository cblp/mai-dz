-- TODO WorkOrder tree

{-# LANGUAGE NamedFieldPuns #-}

import           Prelude hiding (product)

import           Control.Monad (void)
import           Data.Char (toLower)
import           Data.Foldable (for_)
import           Data.List (isInfixOf)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           System.Environment (getArgs)

import           QAbstractItemView
import           QApplication
import           QCoreApplication
import           QLineEdit
import           QMainWindow
import           QTabWidget
import           QToolBar
import           QTreeView
import           QTreeWidget
import           QTreeWidgetItem
import           QWidget (QWidget, cast, showMaximized)
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
    window <- QMainWindow.new
    setUnifiedTitleAndToolBarOnMac window True

    tabs     <- QTabWidget.new
    products <- makeProductView
    _        <- addTab tabs products "Products"
    setCentralWidget window tabs

    do
        toolBar <- addToolBarWithTitle window ""
        search  <- QLineEdit.new
        setPlaceholderText    search "Фильтр..."
        setClearButtonEnabled search True
        connect_ search textChangedSignal (updateProductViewWithSearch products)
        void (addWidget toolBar search)

    pure (QWidget.cast window)

makeProductView :: IO QTreeWidget
makeProductView = do
    productView <- QTreeWidget.new
    setAlternatingRowColors productView True

    setHeaderLabels productView (map (Text.unpack . unDBName . fieldDB) fields)

    products <- runDB (selectList [] [])
    for_ products $ \(Entity _productId product) -> do
        row <- case toPersistValue (product :: Product) of
            PersistList row -> pure row
            PersistMap  row -> pure (map snd row)
            value           -> error (show value)
        labels <- for row $ \item -> case item of
            PersistNull -> pure ""
            _           -> case fromPersistValueText item of
                Left  e -> error (Text.unpack e)
                Right r -> pure (Text.unpack r)
        QTreeWidgetItem.newWithParentTreeAndStrings productView labels
    for_ [0 .. length fields - 1] (resizeColumnToContents productView)

    -- In order to avoid performance issues, it is recommended that sorting is
    -- enabled after inserting the items into the tree.
    setSortingEnabled productView True

    pure productView
    where fields = entityFields (entityDef (Proxy :: Proxy Product))

updateProductViewWithSearch :: QTreeWidget -> String -> IO ()
updateProductViewWithSearch productView searchTerms = do
    products <- topLevelItemCount productView
    for_ [0 .. products - 1] $ \i -> do
        item    <- topLevelItem productView i
        matched <- case searchTerms of
            "" -> pure True
            _  -> do
                columns <- columnCount item
                matched <- for [0 .. columns - 1] $ \j -> do
                    cellText <- QTreeWidgetItem.text item j
                    pure (match cellText)
                pure (or matched)
        setHidden item (not matched)
    where match txt = map toLower searchTerms `isInfixOf` map toLower txt
