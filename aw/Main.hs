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
import           Graphics.UI.Qtah.Widgets.QTabWidget (addTab)
import qualified Graphics.UI.Qtah.Widgets.QTabWidget as QTabWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidget (setHeaderLabels)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

import           DB (Entity (Entity),
                     PersistValue (PersistList, PersistMap, PersistNull),
                     Product, entityDef, entityFields, fieldDB,
                     fromPersistValueText, runDB, selectList, toPersistValue,
                     unDBName)

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeMainWindow
    QWidget.showMaximized mainWindow
    exec
    where withApp = withScopedPtr $ getArgs >>= QApplication.new

makeMainWindow :: IO QWidget
makeMainWindow = do
    tabs <- QTabWidget.new
    let tabs' = QWidget.cast tabs
    productView <- makeProductView tabs'
    void $ addTab tabs productView "Products"
    pure tabs'

makeProductView :: QWidget -> IO QWidget
makeProductView parent = do
    productView <- QTreeWidget.newWithParent parent

    setHeaderLabels productView
        $ map (Text.unpack . unDBName . fieldDB)
        $ entityFields
        $ entityDef (Proxy :: Proxy Product)

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

    pure $ QWidget.cast productView
