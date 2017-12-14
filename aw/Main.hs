{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import           Prelude hiding (product)

import           Control.Monad (void)
import           Data.Foldable (for_)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import           Graphics.UI.Qtah.Widgets.QMainWindow (addToolBarWithTitle,
                                                       setCentralWidget,
                                                       setUnifiedTitleAndToolBarOnMac)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import           Graphics.UI.Qtah.Widgets.QTabWidget (addTab)
import qualified Graphics.UI.Qtah.Widgets.QTabWidget as QTabWidget
import           Graphics.UI.Qtah.Widgets.QToolBar (addWidget)
import           Graphics.UI.Qtah.Widgets.QTreeView (resizeColumnToContents)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (setHeaderLabels)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

import           DB

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeMainWindow
    QWidget.showMaximized mainWindow
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
