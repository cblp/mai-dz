{-# LANGUAGE NamedFieldPuns #-}

import           Prelude hiding (product)

import qualified Data.Text as Text
import           Data.Foldable (for_)
import           Data.Proxy (Proxy (Proxy))
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget,
                                                       setHeaderLabels)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

import           DB (DBName (DBName), Entity (Entity), FieldDef (FieldDef),
                     PersistValue (PersistList, PersistMap), Product, entityDef,
                     entityFields, fieldDB, runDB, selectList, toPersistValue)

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeProductView
    QWidget.showMaximized mainWindow
    exec
    where withApp = withScopedPtr $ getArgs >>= QApplication.new

makeProductView :: IO QTreeWidget
makeProductView = do
    productView <- QTreeWidget.new

    setHeaderLabels
        productView
        [ Text.unpack name
        | FieldDef { fieldDB = DBName name } <- entityFields
            $ entityDef (Proxy :: Proxy Product)
        ]

    products <- runDB $ selectList [] []
    for_ products $ \(Entity _productId product) -> do
        row <- case toPersistValue (product :: Product) of
            PersistList row -> pure row
            PersistMap  row -> pure $ map snd row
            value           -> error $ show value
        QTreeWidgetItem.newWithParentTreeAndStrings productView (map show row)

    pure productView
