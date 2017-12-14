{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import           Prelude hiding (product)

import           Control.Monad (void)
import           Data.Foldable (for_)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           System.Environment (getArgs)

import           QApplication
import           QCoreApplication
import           QLabel
import           QLineEdit
import           QMainWindow
import           QTabWidget
import           QToolBar
import           QTreeView
import           QTreeWidget
import           QTreeWidgetItem
import           QWidget (QWidget, cast, showMaximized)

import           DB

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeMainWindow
    showMaximized mainWindow
    exec
    where withApp = withScopedPtr $ getArgs >>= QApplication.new

makeMainWindow :: IO QWidget
makeMainWindow = do
    window <- QMainWindow.new
    setUnifiedTitleAndToolBarOnMac window True

    toolBar <- addToolBarWithTitle window ""
    void $ addWidget toolBar =<< QLabel.newWithText "Поиск"
    void $ addWidget toolBar =<< QLineEdit.new

    tabs     <- QTabWidget.new
    products <- makeProductView
    void $ addTab tabs products "Products"
    setCentralWidget window tabs

    pure $ QWidget.cast window

makeProductView :: IO QWidget
makeProductView = do
    productView <- QTreeWidget.new
    setHeaderLabels productView $ map (Text.unpack . unDBName . fieldDB) fields
    products <- runDB $ selectList [] []
    for_ products $ \(Entity _productId product) -> do
        row <- case toPersistValue (product :: Product) of
            PersistList row -> pure row
            PersistMap  row -> pure $ map snd row
            value           -> error $ show value
        labels <- for row $ \case
            PersistNull -> pure ""
            item        -> case fromPersistValueText item of
                Left  e -> error $ Text.unpack e
                Right r -> pure $ Text.unpack r
        QTreeWidgetItem.newWithParentTreeAndStrings productView labels
    for_ [0 .. length fields - 1] $ resizeColumnToContents productView
    pure $ QWidget.cast productView
    where fields = entityFields $ entityDef (Proxy :: Proxy Product)
