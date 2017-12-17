-- TODO WorkOrder tree

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad (void)
import           Data.Char (toLower)
import           Data.Foldable (for_)
import           Data.List (isInfixOf)
import           Data.Proxy (Proxy)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Foreign.Hoppy.Runtime (nullptr, withScopedPtr)
import           System.Environment (getArgs)

import           QAbstractItemView
import           QAction
import           QApplication
import           QBoxLayout
import           QCoreApplication
import           QLineEdit
import           QMainWindow
import           QTabWidget
import           QtCore (QtContextMenuPolicy (ActionsContextMenu))
import           QTreeView
import           QTreeWidget
import           QTreeWidgetItem
import           QVBoxLayout
import           QWidget (QWidget, cast, insertAction, setContextMenuPolicy,
                          setLayout, showMaximized)
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
    window <- QMainWindow.new
    setUnifiedTitleAndToolBarOnMac window True

    tabs <- QTabWidget.new
    do
        productView <- addTableTab tabs "Продукция (Product)" pProduct
        setContextMenuPolicy productView ActionsContextMenu
        insertAction         productView (nullptr :: QAction)
            =<< makeAction (print True)
    setCentralWidget window tabs

    pure (QWidget.cast window)
  where
    addTableTab tabs name pTable = do
        (tab, view) <- makeTableTab pTable
        void $ addTab tabs tab name
        pure view

makeTableTab :: SqlTable record => Proxy record -> IO (QWidget, QTreeWidget)
makeTableTab pTable = do
    tab <- QWidget.new
    box <- QVBoxLayout.new
    setLayout tab box

    search <- QLineEdit.new
    setPlaceholderText    search "Фильтр..."
    setClearButtonEnabled search True
    addWidget             box    search

    view <- makeTableView pTable
    addWidget box    view

    connect_  search textChangedSignal $ updateViewWithSearch view

    pure (tab, view)

makeTableView
    :: forall record . SqlTable record => Proxy record -> IO QTreeWidget
makeTableView pTable = do
    view <- QTreeWidget.new
    setAlternatingRowColors view True

    setHeaderLabels view $ map (Text.unpack . unDBName . fieldDB) fields

    items :: [Entity record] <- runDB (selectList [] [])
    for_ items $ \(Entity _id record) -> do
        row <- case toPersistValue record of
            PersistList row -> pure row
            PersistMap  row -> pure $ map snd row
            value           -> error $ show value
        labels <- for row $ \item -> case item of
            PersistNull -> pure ""
            _           -> case fromPersistValueText item of
                Left  e -> error $ Text.unpack e
                Right r -> pure $ Text.unpack r
        QTreeWidgetItem.newWithParentTreeAndStrings view labels
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

makeAction :: IO () -> IO QAction
makeAction handler = do
    action <- QAction.newWithText "Заказы (WorkOrder)"
    connect_ action triggeredSignal $ const handler
    pure action
